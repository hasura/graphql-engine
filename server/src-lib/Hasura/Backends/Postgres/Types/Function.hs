-- | Postgres Types Functions
--
-- Postgres specific types related to SQL functions
module Hasura.Backends.Postgres.Types.Function
  ( ArgumentExp (..),
    onArgumentExp,
    actionResponsePayloadColumn,
    FunctionArg (..),
    HasDefault (..),
  )
where

import Data.Aeson
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Function.Cache
import Hasura.Prelude

newtype HasDefault = HasDefault {unHasDefault :: Bool}
  deriving (Show, Eq, Ord, Generic, ToJSON, NFData, Hashable)

data FunctionArg = FunctionArg
  { faName :: Maybe FunctionArgName,
    faType :: QualifiedPGType,
    faHasDefault :: HasDefault
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData FunctionArg

instance Hashable FunctionArg

instance ToJSON FunctionArg where
  toJSON = genericToJSON hasuraJSON

-- Function arguments

data ArgumentExp a
  = -- | Table row accessor
    AETableRow
  | -- | Hardcoded reference to @hdb_catalog.hdb_action_log.response_payload@
    AEActionResponsePayload
  | -- | JSON/JSONB hasura session variable object
    AESession a
  | AEInput a
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (Hashable a) => Hashable (ArgumentExp a)

-- | Eliminate 'ArgumentExp'
--
-- Used to ensure that the right column is used for 'AEActionResponsePayload'.
onArgumentExp ::
  -- | Value to return for 'AETableRow'
  a ->
  -- | Create value to return for 'AEResponsePayload' given column text
  (Text -> a) ->
  -- | 'ArgumentExp' to eliminate
  ArgumentExp a ->
  a
onArgumentExp tableRow fromColumn = \case
  AETableRow -> tableRow
  AEActionResponsePayload -> fromColumn actionResponsePayloadColumn
  AESession a -> a
  AEInput a -> a

-- | Hardcoded @hdb_catalog.hdb_action_log.response_payload@ column name
actionResponsePayloadColumn :: Text
actionResponsePayloadColumn = "response_payload"
{-# INLINE actionResponsePayloadColumn #-}
