module Hasura.Backends.SQLite.Translate where

import           Hasura.Prelude

import qualified Data.Aeson                         as J
import qualified Data.Text                          as T
import qualified Data.Text.Extended                 as T
import qualified Database.SQLite.Simple             as L

import           Data.Text.Extended                 ((<>>))
import           Data.Text.Lazy                     (toStrict)
import           Data.Text.Lazy.Encoding            (decodeUtf8)

import qualified Hasura.RQL.IR                      as IR
import qualified Hasura.RQL.Types.Column            as RQL
import qualified Hasura.RQL.Types.Common            as RQL

import           Hasura.Backends.Postgres.SQL.Types (pgFmtLit)
import           Hasura.Base.Error
import           Hasura.GraphQL.Parser
import           Hasura.SQL.Backend
import           Hasura.Session


-- Planning

planInline
  :: MonadError QErr m
  => SessionVariables
  -> IR.QueryDB 'SQLite (Const Void) (UnpreparedValue 'SQLite)
  -> m L.Query
planInline sessionVariables = IR.traverseQueryDB inline >=> \case
  IR.QDBMultipleRows select -> fromSelect select
  _                         -> throw500 $ "only regular selects are implemented for now"
  where
    inline = \case
      UVLiteral x      -> pure x
      UVSession        -> pure $ toStrict $ decodeUtf8 $ J.encode sessionVariables
      UVParameter _ p  -> pure $ RQL.cvValue p
      UVSessionVar _ v -> getSessionVariableValue v sessionVariables
                          `onNothing` throw400 NotFound ("missing session variable: " <>> v)

-- Translation

fromSelect
  :: MonadError QErr m => IR.AnnSelectG 'SQLite (Const Void) (IR.AnnFieldsG 'SQLite (Const Void) Text) Text
  -> m L.Query
fromSelect (IR.AnnSelectG fields from _ args _) = do
  case from of
    IR.FromFunction {}     -> throw500 "functions"
    IR.FromTable tableName -> do
      fs <- fromFields fields
      we <- traverse fromBoolExp (IR._saWhere args)
      pure $ L.Query $ mconcat
        [ "select json_object("
        , mkFields fs
        , ") from "
        , tableName
        , " where "
        , fromMaybe "TRUE" we
        ]
  where
    mkFields fs = T.commaSeparated $ do
      (alias, f) <- fs
      [T.squote alias, f]

fromFields
  :: MonadError QErr m => IR.AnnFieldsG 'SQLite (Const Void) Text
  -> m [(Text, Text)]
fromFields = traverse \(RQL.FieldName fieldName, fieldInfo) -> case fieldInfo of
  IR.AFColumn columnField -> do
    pure (fieldName, RQL.pgiColumn $ IR._acfInfo columnField)
  _ -> throw500 "non-column field"


fromBoolExp
  :: MonadError QErr m => IR.AnnBoolExp 'SQLite Text
  -> m Text
fromBoolExp = \case
  IR.BoolExists _ -> throw500 "not implemented yet"
  IR.BoolAnd e    -> undefined -- TODO
  IR.BoolOr  e    -> undefined -- TODO
  IR.BoolNot e    -> undefined -- TODO
  IR.BoolFld f    -> case f of
    IR.AVRel _ _    -> throw500 "not implemented yet"
    IR.AVCol RQL.ColumnInfo{ pgiColumn = col} ops -> do
      joinWith " AND " "TRUE" <$> for ops \case
        IR.AEQ True  a -> pure $ mconcat [col, " == ", pgFmtLit a]
        IR.AEQ False a -> pure $ mconcat [col, " != ", pgFmtLit a]
        IR.ANE True  a -> pure $ mconcat [col, " == ", pgFmtLit a]
        IR.ANE False a -> pure $ mconcat [col, " != ", pgFmtLit a]

        IR.AIN  a      -> pure $ mconcat [        col, " IN ",  a]
        IR.ANIN a      -> pure $ mconcat ["NOT ", col, " IN ",  a]

        IR.AGT    a    -> pure $ mconcat [col, " > ",  pgFmtLit a]
        IR.ALT    a    -> pure $ mconcat [col, " < ",  pgFmtLit a]
        IR.AGTE   a    -> pure $ mconcat [col, " >= ", pgFmtLit a]
        IR.ALTE   a    -> pure $ mconcat [col, " <= ", pgFmtLit a]

        IR.ALIKE  a    -> pure $ mconcat [        col, " LIKE ", pgFmtLit a]
        IR.ANLIKE a    -> pure $ mconcat ["NOT ", col, " LIKE ", pgFmtLit a]

        IR.ANISNULL    -> pure $ col <> " ISNULL"
        IR.ANISNOTNULL -> pure $ col <> " NOTNULL"
        _              -> throw500 "not implemented yet"
  where
    joinWith op def l = case l of
      [] -> def
      _  -> "(" <> T.intercalate op l <> ")"
