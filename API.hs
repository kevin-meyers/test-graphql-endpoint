{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

module API (api, main) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus (interpreter)
import Data.Morpheus.Types (RootResolver (..), Undefined (..), GQLType, ResolverQ)
import Data.Text (Text)
import GHC.Generics (Generic)

import Control.Monad.Trans.Reader (ReaderT)

import Control.Monad.IO.Class (liftIO)

import Web.Scotty

import Database.Persist
import Database.Persist.MySQL --(createMySQLPool, myConnInfo, myPoolSize, runSqlPool)
import Database.Persist.TH

import Data.Time.Clock (UTCTime)
import GHC.Word (Word16)

import Control.Monad.Logger (runNoLoggingT)

import Database.MySQL.Base.Types (Protocol (TCP), Option (Protocol) )

connInfo :: ConnectInfo
connInfo = ConnectInfo "127.0.0.1" (3306 :: Word16) "root" "" "clearvoice_development" [Protocol TCP] "" Nothing

-- This will connect to the mysql instance defined in connInfo and run the action
runDB :: ReaderT SqlBackend IO a -> IO a
runDB action = do
    pool <- runNoLoggingT $ createMySQLPool connInfo 2
    runSqlPool action pool

-- Defining the database tables (so far I only added labels, but its easy to add more)
-- The TH (template haskell) generates functions and data types for us such as
-- Label, the type and constructor
-- LabelID type and value which is used as a key to do table lookups
-- it also makes accessor functions for each of the 
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Label sql=labels
    name Text
    uid Text
    accountId Int
    UniquePair name accountId
    createdAt UTCTime
    updatedAt UTCTime
    deriving Show
|]

-- The data type that described the possible queries
data Query m = Query
  { label :: LabelArgs -> m PublicLabel
  } deriving (Generic, GQLType)

-- The data type that holds all the label data we possibly want to return
data PublicLabel = PublicLabel
  { name :: Text
  , accountId :: Int
  , assignmentCount :: Int
  } deriving (Generic, GQLType)

-- The data type that holds all the args we possibly want to accept
data LabelArgs = LabelArgs
  { name      :: Text
  , accountId :: Int -- In the future this will be of type AccountId if that table were mapped here
  } deriving (Generic, GQLType)


-- I had to use rawSql here because I didnt want to add the data type for assignments_labels yet
assignmentCountForLabel :: LabelId -> IO Int
assignmentCountForLabel labelId = length <$> (runDB $ rawSql ("select ?? from labels, assignments_labels where label_id=?" :: Text) [toPersistValue labelId] :: IO [Entity Label])

-- Takes in the args, returns either a PublicLabel or an error 
resolveLabel :: LabelArgs -> ResolverQ e IO PublicLabel
resolveLabel LabelArgs {name, accountId} = do
    -- finds a label by the UniquePair of name and accountId
    -- then runs the action, and lifts it out of IO into the "Either" like monad
    maybeLabel <- liftIO $ runDB $ getBy $ UniquePair name accountId
    -- Handling the possibility of failure
    case maybeLabel of
        Nothing -> fail "No label found for that pair"

        -- run the PublicLabel through pure to lift it into the "Either" like monad
        Just (Entity labelId label) -> do
            assignmentCount <- liftIO $ assignmentCountForLabel labelId
            pure $ PublicLabel
                { name = labelName label
                , accountId = labelAccountId label
                , assignmentCount = assignmentCount
                }

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {label = resolveLabel},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

api :: ByteString -> IO ByteString
api = interpreter rootResolver

main :: IO ()
main = scotty 5500 $ post "/api" $ raw =<< (liftIO . api =<< body)
