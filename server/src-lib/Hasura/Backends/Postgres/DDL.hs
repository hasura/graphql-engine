-- | Postgres DDL
--
-- Implements the DDL related methods of the 'Hasura.RQL.Types.Metadata.Backend.BackendMetadata'
-- type class for the Postgres backend, which provides an interface for fetching information about
-- the objects in the database, such as tables, relationships, etc.
module Hasura.Backends.Postgres.DDL
  ( parseCollectableType,
    module M,
  )
where

import Data.Aeson
import Hasura.Backends.Postgres.DDL.BoolExp as M
import Hasura.Backends.Postgres.DDL.ComputedField as M
import Hasura.Backends.Postgres.DDL.EventTrigger as M
import Hasura.Backends.Postgres.DDL.Function as M
import Hasura.Backends.Postgres.DDL.Source as M
import Hasura.Backends.Postgres.DDL.Table as M
import Hasura.Backends.Postgres.SQL.DML
import Hasura.Backends.Postgres.Translate.Column
import Hasura.Backends.Postgres.Types.Column
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.SQL.Types
import Hasura.Server.Utils
import Hasura.Session

parseCollectableType ::
  forall pgKind m.
  (Backend ('Postgres pgKind), MonadError QErr m) =>
  CollectableType (ColumnType ('Postgres pgKind)) ->
  Value ->
  m (PartialSQLExp ('Postgres pgKind))
parseCollectableType pgType = \case
  -- When it is a special variable
  String t
    | isSessionVariable t -> return $ mkTypedSessionVar pgType $ mkSessionVariable t
    | isReqUserId t -> return $ mkTypedSessionVar pgType userIdHeader
  -- Typical value as Aeson's value
  val -> case pgType of
    CollectableTypeScalar cvType ->
      PSESQLExp . toTxtValue . ColumnValue cvType <$> parseScalarValueColumnTypeWithContext () cvType val
    CollectableTypeArray ofType -> do
      vals <- runAesonParser parseJSON val
      scalarValues <- parseScalarValuesColumnTypeWithContext () ofType vals
      return
        . PSESQLExp
        $ SETyAnn
          (SEArray $ map (toTxtValue . ColumnValue ofType) scalarValues)
          (mkTypeAnn $ CollectableTypeArray (unsafePGColumnToBackend ofType))

mkTypedSessionVar ::
  CollectableType (ColumnType ('Postgres pgKind)) ->
  SessionVariable ->
  PartialSQLExp ('Postgres pgKind)
mkTypedSessionVar columnType =
  PSESessVar (unsafePGColumnToBackend <$> columnType)
