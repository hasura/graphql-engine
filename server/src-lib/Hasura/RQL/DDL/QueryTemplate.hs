module Hasura.RQL.DDL.QueryTemplate
  ( createQueryTemplateP1
  , createQueryTemplateP2
  , delQTemplateFromCatalog
  , TemplateParamConf(..)
  , CreateQueryTemplate(..)
  , runCreateQueryTemplate
  , QueryTP1

  , DropQueryTemplate(..)
  , runDropQueryTemplate

  , SetQueryTemplateComment(..)
  , runSetQueryTemplateComment
  ) where

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DML.Internal    (sessVarFromCurrentSetting)
import           Hasura.RQL.GBoolExp        (txtRHSBuilder)
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Database.PG.Query          as Q
import qualified Hasura.RQL.DML.Count       as R
import qualified Hasura.RQL.DML.Delete      as R
import qualified Hasura.RQL.DML.Insert      as R
import qualified Hasura.RQL.DML.Select      as R
import qualified Hasura.RQL.DML.Update      as R
import qualified Hasura.SQL.DML             as PS

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.HashMap.Strict        as M
import qualified Data.Text                  as T

data TemplateParamConf
  = TemplateParamConf
  { tpcParam   :: !TemplateParam
  , tpcDefault :: !(Maybe Value)
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''TemplateParamConf)

data CreateQueryTemplate
  = CreateQueryTemplate
  { cqtName     :: !TQueryName
  , cqtTemplate :: !QueryT
  , cqtComment  :: !(Maybe T.Text)
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''CreateQueryTemplate)

validateParam
  :: (QErrM m)
  => PGColType
  -> Value
  -> m PS.SQLExp
validateParam pct val =
  case val of
    Object _ -> do
      tpc <- decodeValue val
      withPathK "default" $
        maybe (return ()) validateDefault $ tpcDefault tpc
      return $ PS.SELit "NULL"
    _ -> txtRHSBuilder pct val
  where
    validateDefault =
      void . runAesonParser (convToBin pct)

mkSelQ :: (QErrM m) => SelectQueryT -> m SelectQuery
mkSelQ (DMLQuery tn (SelectG c w o lim offset)) = do
  intLim <- withPathK "limit" $ maybe returnNothing parseAsInt lim
  intOffset <- withPathK "offset" $ maybe returnNothing parseAsInt offset
  return $ DMLQuery tn $ SelectG c w o intLim intOffset
  where
    returnNothing = return Nothing
    parseAsInt v = case v of
      Object _ -> do
        tpc <- decodeValue v
        withPathK "default" $
          mapM decodeValue $ tpcDefault tpc
      _ -> Just <$> decodeValue v

data QueryTP1
  = QTP1Insert R.InsertQueryP1
  | QTP1Select R.AnnSimpleSel
  | QTP1Update R.AnnUpd
  | QTP1Delete R.AnnDel
  | QTP1Count R.CountQueryP1
  | QTP1Bulk [QueryTP1]
  deriving (Show, Eq)

validateTQuery
  :: (QErrM m, UserInfoM m, CacheRM m, HasSQLGenCtx m)
  => QueryT
  -> m QueryTP1
validateTQuery qt = withPathK "args" $ case qt of
  QTInsert q -> QTP1Insert <$>
    R.convInsertQuery decodeInsObjs sessVarFromCurrentSetting validateParam q
  QTSelect q -> QTP1Select <$>
    (mkSelQ q >>= R.convSelectQuery sessVarFromCurrentSetting validateParam)
  QTUpdate q -> QTP1Update <$>
    R.validateUpdateQueryWith sessVarFromCurrentSetting validateParam q
  QTDelete q -> QTP1Delete <$>
    R.validateDeleteQWith sessVarFromCurrentSetting validateParam q
  QTCount q  -> QTP1Count  <$>
    R.validateCountQWith sessVarFromCurrentSetting validateParam q
  QTBulk q   -> QTP1Bulk   <$> mapM validateTQuery q
  where
    decodeInsObjs val = do
      tpc <- decodeValue val
      mDefObjs <- mapM decodeValue $ tpcDefault tpc
      return $ fromMaybe [] mDefObjs

collectDeps
  :: QueryTP1 -> [SchemaDependency]
collectDeps qt = case qt of
  QTP1Insert qp1 -> R.getInsertDeps qp1
  QTP1Select qp1 -> R.getSelectDeps qp1
  QTP1Update qp1 -> R.getUpdateDeps qp1
  QTP1Delete qp1 -> R.getDeleteDeps qp1
  QTP1Count qp1  -> R.getCountDeps qp1
  QTP1Bulk qp1   -> concatMap collectDeps qp1

createQueryTemplateP1
  :: (UserInfoM m, QErrM m, CacheRM m, HasSQLGenCtx m)
  => CreateQueryTemplate
  -> m (WithDeps QueryTemplateInfo)
createQueryTemplateP1 (CreateQueryTemplate qtn qt _) = do
  adminOnly
  sc <- askSchemaCache
  withPathK "name" $ when (isJust $ M.lookup qtn $ scQTemplates sc) $
    throw400 AlreadyExists $ "the query template already exists : " <>> qtn
  qtp1 <- withPathK "template" $ liftP1 $ validateTQuery qt
  let deps = collectDeps qtp1
  return (QueryTemplateInfo qtn qt, deps)

addQTemplateToCatalog
  :: CreateQueryTemplate
  -> Q.TxE QErr ()
addQTemplateToCatalog (CreateQueryTemplate qtName qtDef mComment) =
  Q.unitQE defaultTxErrorHandler [Q.sql|
           INSERT INTO
                  hdb_catalog.hdb_query_template
                  (template_name, template_defn, comment)
           VALUES ($1, $2 :: jsonb, $3)
                |] (qtName, Q.AltJ qtDef, mComment) False

createQueryTemplateP2
  :: (QErrM m, CacheRWM m, MonadTx m)
  => CreateQueryTemplate
  -> WithDeps QueryTemplateInfo
  -> m EncJSON
createQueryTemplateP2 cqt (qti, deps) = do
  addQTemplateToCache qti deps
  liftTx $ addQTemplateToCatalog cqt
  return successMsg

runCreateQueryTemplate
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m, HasSQLGenCtx m)
  => CreateQueryTemplate -> m EncJSON
runCreateQueryTemplate q =
  createQueryTemplateP1 q >>= createQueryTemplateP2 q

data DropQueryTemplate
  = DropQueryTemplate
  { dqtName :: !TQueryName
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''DropQueryTemplate)

delQTemplateFromCatalog
  :: TQueryName
  -> Q.TxE QErr ()
delQTemplateFromCatalog qtn =
  Q.unitQE defaultTxErrorHandler [Q.sql|
           DELETE FROM
                  hdb_catalog.hdb_query_template
           WHERE template_name =  $1
                |] (Identity qtn) False

runDropQueryTemplate
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m)
  => DropQueryTemplate -> m EncJSON
runDropQueryTemplate q = do
  withPathK "name" $ void $ askQTemplateInfo qtn
  delQTemplateFromCache qtn
  liftTx $ delQTemplateFromCatalog qtn
  return successMsg
  where
    qtn = dqtName q

data SetQueryTemplateComment
  = SetQueryTemplateComment
  { sqtcName    :: !TQueryName
  , sqtcComment :: !(Maybe T.Text)
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 4 snakeCase) ''SetQueryTemplateComment)

setQueryTemplateCommentP1
  :: (UserInfoM m, QErrM m, CacheRM m)
  => SetQueryTemplateComment -> m ()
setQueryTemplateCommentP1 (SetQueryTemplateComment qtn _) = do
  adminOnly
  void $ askQTemplateInfo qtn

setQueryTemplateCommentP2
  :: (QErrM m, MonadTx m) => SetQueryTemplateComment -> m EncJSON
setQueryTemplateCommentP2 apc = do
  liftTx $ setQueryTemplateCommentTx apc
  return successMsg

setQueryTemplateCommentTx
  :: SetQueryTemplateComment
  -> Q.TxE QErr ()
setQueryTemplateCommentTx (SetQueryTemplateComment qtn comment) =
  Q.unitQE defaultTxErrorHandler
  [Q.sql|
    UPDATE hdb_catalog.hdb_query_template
    SET comment = $1
    WHERE template_name =  $2
        |] (comment, qtn) False

runSetQueryTemplateComment
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m)
  => SetQueryTemplateComment -> m EncJSON
runSetQueryTemplateComment q = do
  setQueryTemplateCommentP1 q
  setQueryTemplateCommentP2 q
