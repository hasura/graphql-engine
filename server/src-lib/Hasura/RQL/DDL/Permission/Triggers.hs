module Hasura.RQL.DDL.Permission.Triggers where

import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Utils
import           Hasura.SQL.Types

import qualified Database.PG.Query   as Q
import qualified Hasura.SQL.DML      as S

import qualified Data.Aeson          as J
import qualified Data.FileEmbed      as FE
import qualified Data.Text           as T

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

getInsTrigTmplt :: (MonadError QErr m) => m GingerTmplt
getInsTrigTmplt =
  either throwErr return $ parseGingerTmplt trigFnSrc
  where
    trigFnSrc = $(FE.embedStringFile "src-rsr/insert_trigger.sql.j2")

    throwErr e = throw500 $ "cannot render insert trigger function template: "
                 <> T.pack e

buildInsTrigFn
  :: (MonadError QErr m)
  => QualifiedTable -> QualifiedTable -> S.BoolExp -> m Q.Query
buildInsTrigFn fn tn be = do
  insTmplt <- getInsTrigTmplt
  return $ Q.fromText $ renderGingerTmplt tmpltVals insTmplt
  where
    tmpltVals = J.object [ "function_name" J..= toSQLTxt fn
                         , "table_name" J..= toSQLTxt tn
                         , "check_expression" J..= toSQLTxt be
                         ]
