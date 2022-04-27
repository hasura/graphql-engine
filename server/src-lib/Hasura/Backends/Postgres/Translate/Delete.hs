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
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Delete
import Hasura.RQL.Types.Backend
import Hasura.SQL.Backend

mkDelete ::
  Backend ('Postgres pgKind) =>
  AnnDel ('Postgres pgKind) ->
  S.SQLDelete
mkDelete (AnnDel tn (fltr, wc) _ _) =
  S.SQLDelete tn Nothing tableFltr $ Just S.returningStar
  where
    tableFltr =
      Just $
        S.WhereFrag $
          toSQLBoolExp (S.QualTable tn) $ andAnnBoolExps fltr wc
