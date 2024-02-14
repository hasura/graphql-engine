-- | Postgres Translate Delete
--
-- Tranlates an IR delete term to a Postgres SQL DELETE statement.
--
-- See 'Hasura.Backends.Postgres.Execute.Mutation.execDeleteQuery'.
module Hasura.Backends.Postgres.Translate.Delete
  ( mkDelete,
  )
where

import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.Translate.BoolExp
import Hasura.Base.Error (QErr)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Delete
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Session (UserInfo)

mkDelete ::
  (Backend ('Postgres pgKind), MonadIO m, MonadError QErr m) =>
  UserInfo ->
  AnnDel ('Postgres pgKind) ->
  m S.SQLDelete
mkDelete userInfo (AnnDel tn (fltr, wc) _ _ _ _ _) = do
  boolExp <- toSQLBoolExp userInfo (S.QualTable tn) $ andAnnBoolExps fltr wc
  let tableFltr =
        Just
          . S.WhereFrag
          . S.simplifyBoolExp
          $ boolExp
  pure $ S.SQLDelete tn Nothing tableFltr $ Just S.returningStar
