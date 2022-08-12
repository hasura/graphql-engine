{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataConnector.IR.Scalar.Value
  ( Value (..),
    Literal (..),
    parseValue,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types qualified as J
import Data.Scientific
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.IR.Scalar.Type qualified as IR.S.T
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Witch qualified

--------------------------------------------------------------------------------

-- | Literal scalar values that can appear as leaf nodes in expressions
--
-- NOTE: This type shouldn't _need_ ser/de instances, but they're imposed by
-- the 'Backend' class.
data Value
  = String Text
  | Number Scientific
  | Boolean Bool
  | Null
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance Witch.From API.Value Value where
  from = \case
    API.String txt -> String txt
    API.Number x -> Number x
    API.Boolean p -> Boolean p
    API.Null -> Null

instance Witch.From Value API.Value where
  from = \case
    String txt -> API.String txt
    Number x -> API.Number x
    Boolean p -> API.Boolean p
    Null -> API.Null

data Literal
  = ValueLiteral Value
  | ArrayLiteral [Value]
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (Cacheable, Hashable, NFData, ToJSON)

parseValue :: IR.S.T.Type -> J.Value -> J.Parser Value
parseValue type' val =
  case (type', val) of
    (_, J.Null) -> pure Null
    (IR.S.T.String, value) -> String <$> J.parseJSON value
    (IR.S.T.Bool, value) -> Boolean <$> J.parseJSON value
    (IR.S.T.Number, value) -> Number <$> J.parseJSON value
