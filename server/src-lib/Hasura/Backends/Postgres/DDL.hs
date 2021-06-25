module Hasura.Backends.Postgres.DDL
  ( parseCollectableType
  , module M
  )
where

import           Data.Aeson

import           Hasura.Backends.Postgres.DDL.BoolExp      as M
import           Hasura.Backends.Postgres.DDL.Field        as M
import           Hasura.Backends.Postgres.DDL.Function     as M
import           Hasura.Backends.Postgres.DDL.Source       as M
import           Hasura.Backends.Postgres.DDL.Table        as M
import           Hasura.Backends.Postgres.SQL.DML
import           Hasura.Backends.Postgres.Translate.Column
import           Hasura.Backends.Postgres.Types.Column
import           Hasura.Base.Error
import           Hasura.Prelude
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Column
import           Hasura.SQL.Backend
import           Hasura.SQL.Types
import           Hasura.Server.Utils
import           Hasura.Session


parseCollectableType
  :: forall pgKind m
   . (Backend ('Postgres pgKind), MonadError QErr m)
  => CollectableType (ColumnType ('Postgres pgKind))
  -> Value
  -> m (PartialSQLExp ('Postgres pgKind))
parseCollectableType pgType = \case
  -- When it is a special variable
  String t
    | isSessionVariable t -> return $ mkTypedSessionVar pgType $ mkSessionVariable t
    | isReqUserId t       -> return $ mkTypedSessionVar pgType userIdHeader
  -- Typical value as Aeson's value
  val -> case pgType of
    CollectableTypeScalar cvType ->
      PSESQLExp . toTxtValue . ColumnValue cvType <$> parseScalarValueColumnType cvType val
    CollectableTypeArray ofType -> do
      vals <- runAesonParser parseJSON val
      scalarValues <- parseScalarValuesColumnType ofType vals
      return . PSESQLExp $ SETyAnn
        (SEArray $ map (toTxtValue . ColumnValue ofType) scalarValues)
        (mkTypeAnn $ CollectableTypeArray (unsafePGColumnToBackend ofType))

mkTypedSessionVar
  :: CollectableType (ColumnType ('Postgres pgKind))
  -> SessionVariable -> PartialSQLExp ('Postgres pgKind)
mkTypedSessionVar columnType =
  PSESessVar (unsafePGColumnToBackend <$> columnType)
