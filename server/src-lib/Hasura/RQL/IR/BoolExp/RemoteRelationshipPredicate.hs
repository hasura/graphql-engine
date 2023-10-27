module Hasura.RQL.IR.BoolExp.RemoteRelationshipPredicate
  ( RemoteRelSessionVariableORLiteralValue (..),
    RemoteRelRHSFetchWhereExp (..),
    RemoteRelSupportedOp (..),
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Extended (FromJSONKeyValue (..))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Hasura.Prelude
import Hasura.RQL.Types.Session (SessionVariable, parseSessionVariable)

data RemoteRelSessionVariableORLiteralValue
  = RemoteRelSessionVariable SessionVariable
  | RemoteRelLiteralValue Text

instance J.FromJSON RemoteRelSessionVariableORLiteralValue where
  parseJSON v =
    parseSessionVar <|> do
      v' <- J.parseJSON v
      pure (RemoteRelLiteralValue v')
    where
      parseSessionVar = J.withText "RemoteRelSessionVariableORLiteralValue" (\t -> RemoteRelSessionVariable <$> parseSessionVariable t) v

data RemoteRelRHSFetchWhereExp f = RemoteRelRHSFetchWhereExp
  { rrrfweColumnFieldName :: f,
    rrrfweBoolExp :: [RemoteRelSupportedOp RemoteRelSessionVariableORLiteralValue]
  }

-- parses json object of the following template:
{-
{
  "<remote column name>": {
    "_eq": "<some session variable name>"
  }
}
 -}

instance (J.FromJSON f) => J.FromJSON (RemoteRelRHSFetchWhereExp f) where
  parseJSON = J.withObject "RemoteRelationshipPredicateExpression" \o ->
    case KM.toList o of
      [(fieldName, innerObj)] -> do
        remoteRelBoolExp <-
          J.withObject
            "column field"
            ( \o' -> do
                forM (KM.toList o') \(op, value) -> do
                  parseJSONKeyValue (op, value)
            )
            innerObj
        field <- J.parseJSON $ J.String (K.toText fieldName)
        pure $ RemoteRelRHSFetchWhereExp field remoteRelBoolExp
      _ -> fail "Expected the remote relationship expression predicate to be {\"<remote column name>\":{\"_eq\":\"<some session variable name>\"}}"

data RemoteRelSupportedOp field
  = RemoteRelEqOp field
  | RemoteRelNeqOp field
  | RemoteRelGtOp field
  | RemoteRelLtOp field
  | RemoteRelGteOp field
  | RemoteRelLteOp field
  | RemoteRelInOp [field]
  | RemoteRelNinOp [field]
  | RemoteRelLikeOp field
  | RemoteRelNlikeOp field
  | RemoteRelIsNullOp Bool

instance (J.FromJSON f) => FromJSONKeyValue (RemoteRelSupportedOp f) where
  parseJSONKeyValue (op, value) = case op of
    "_eq" -> RemoteRelEqOp <$> J.parseJSON value
    "_neq" -> RemoteRelNeqOp <$> J.parseJSON value
    "_gt" -> RemoteRelGtOp <$> J.parseJSON value
    "_lt" -> RemoteRelLtOp <$> J.parseJSON value
    "_gte" -> RemoteRelGteOp <$> J.parseJSON value
    "_lte" -> RemoteRelLteOp <$> J.parseJSON value
    "_in" -> RemoteRelInOp <$> J.parseJSON value
    "_nin" -> RemoteRelNinOp <$> J.parseJSON value
    "_like" -> RemoteRelLikeOp <$> J.parseJSON value
    "_nlike" -> RemoteRelNlikeOp <$> J.parseJSON value
    "_is_null" -> RemoteRelIsNullOp <$> J.parseJSON value
    _ -> fail "Expected one of the supported operators (_eq, _neq, _gt, _lt, _gte, _lte, _in, _nin, _like, _nlike and _is_null)"
