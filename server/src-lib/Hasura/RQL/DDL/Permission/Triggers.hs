module Hasura.RQL.DDL.Permission.Triggers
  ( buildInsTrig
  , dropInsTrigFn
  , buildInsTrigFn
  ) where

import           Hasura.Prelude
import           Hasura.SQL.Types

import qualified Database.PG.Query     as Q
import qualified Hasura.SQL.DML        as S

import qualified Data.Text.Lazy        as TL
import qualified Text.Shakespeare.Text as ST

buildInsTrig :: QualifiedTable -> Q.Query
buildInsTrig qt@(QualifiedObject _ tn) =
  Q.fromBuilder $ mconcat
  [ "CREATE TRIGGER " <> toSQL tn
  , " INSTEAD OF INSERT ON " <> toSQL qt
  , " FOR EACH ROW EXECUTE PROCEDURE "
  , toSQL qt <> "();"
  ]

dropInsTrigFn :: QualifiedTable -> Q.Query
dropInsTrigFn fn =
  Q.fromBuilder $ "DROP FUNCTION " <> toSQL fn <> "()"

buildInsTrigFn :: QualifiedTable -> QualifiedTable -> S.BoolExp -> Q.Query
buildInsTrigFn fn tn be = Q.fromText . TL.toStrict $
  let functionName = toSQLTxt fn
      tableName = toSQLTxt tn
      checkExpression = toSQLTxt be
  in $(ST.stextFile "src-rsr/insert_trigger.sql.shakespeare")
