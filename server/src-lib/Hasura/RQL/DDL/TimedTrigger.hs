{-# LANGUAGE RecordWildCards #-}

module Hasura.RQL.DDL.TimedTrigger
  ( TimedTriggerQuery(..)
  , runCreateTimedTrigger
  ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema.Cache (CacheBuildM)
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Common     (NonEmptyText)
import           Language.Haskell.TH.Syntax  (Lift)

import qualified Database.PG.Query           as Q

data TimedTriggerQuery
  = TimedTriggerQuery
  { ttqName     :: !NonEmptyText
  , ttqWebhook  :: !NonEmptyText
  , ttqSchedule :: !NonEmptyText
  }
  deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''TimedTriggerQuery)

runCreateTimedTrigger :: CacheBuildM m => TimedTriggerQuery ->  m EncJSON
runCreateTimedTrigger TimedTriggerQuery{..} = do
  liftTx $  Q.unitQE defaultTxErrorHandler
         [Q.sql|
           INSERT into hdb_catalog.hdb_scheduled_trigger
                       (name, webhook, schedule)
           VALUES ($1, $2, $3)
         |] (ttqName, Q.AltJ $ toJSON ttqWebhook, ttqSchedule) False
  return successMsg
