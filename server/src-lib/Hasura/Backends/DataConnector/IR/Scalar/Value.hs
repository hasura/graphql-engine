{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataConnector.IR.Scalar.Value
  ( Literal (..),
    parseValue,
  )
where

import Data.Aeson (ToJSON, Value)
import Data.Aeson.Types qualified as J
import Hasura.Backends.DataConnector.IR.Scalar.Type qualified as IR.S.T
import Hasura.Incremental (Cacheable)
import Hasura.Prelude

data Literal
  = ValueLiteral Value
  | ArrayLiteral [Value]
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (Cacheable, Hashable, NFData, ToJSON)

parseValue :: IR.S.T.Type -> J.Value -> J.Parser Value
parseValue type' val =
  case (type', val) of
    (_, J.Null) -> pure J.Null
    (IR.S.T.String, value) -> J.String <$> J.parseJSON value
    (IR.S.T.Bool, value) -> J.Bool <$> J.parseJSON value
    (IR.S.T.Number, value) -> J.Number <$> J.parseJSON value
    -- For custom scalar types we don't know what subset of JSON values
    -- they accept, so we just accept any value.
    (IR.S.T.Custom _, value) -> pure value
