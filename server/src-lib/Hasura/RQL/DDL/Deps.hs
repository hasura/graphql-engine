module Hasura.RQL.DDL.Deps
       ( purgeRel
       , parseDropNotice
       , getIndirectDeps
       , reportDeps
       , reportDepsExt
       )
       where

import           Hasura.Prelude

import qualified Data.HashSet      as HS
import qualified Data.Text         as T
import qualified Database.PG.Query as Q

import           Hasura.RQL.Types
import           Hasura.SQL.Types

purgeRel :: QualifiedTable -> RelName -> Q.Tx ()
purgeRel (QualifiedObject sn tn) rn =
  Q.unitQ [Q.sql|
           DELETE FROM hdb_catalog.hdb_relationship
                 WHERE table_schema = $1
                   AND table_name = $2
                   AND rel_name = $3
                |] (sn, tn, rn) False

reportDeps :: (QErrM m) => [SchemaObjId] -> m ()
reportDeps deps =
  throw400 DependencyError $
    "cannot drop due to the following dependent objects : "
     <> reportSchemaObjs deps

reportDepsExt :: (QErrM m) => [SchemaObjId] -> [T.Text] -> m ()
reportDepsExt deps unknownDeps =
  throw400 DependencyError $
    "cannot drop due to the following dependent objects : " <> depObjsTxt
  where
    depObjsTxt = T.intercalate ", " (reportSchemaObjs deps:unknownDeps)

parseDropNotice :: (QErrM m ) => T.Text -> m [Either T.Text SchemaObjId]
parseDropNotice t = do
  cascadeLines <- getCascadeLines
  mapM parseCascadeLine cascadeLines
  where
    dottedTxtToQualTable dt =
      case T.split (=='.') dt of
        [tn]     -> return $ QualifiedObject publicSchema $ TableName tn
        [sn, tn] -> return $ QualifiedObject (SchemaName sn) $ TableName tn
        _        -> throw400 ParseFailed $ "parsing dotted table failed : " <> dt

    getCascadeLines = do
      detailLines <- case T.stripPrefix "NOTICE:" t of
        Just rest -> case T.splitOn "DETAIL:" $ T.strip rest of
          [singleDetail] -> return [singleDetail]
          [_, detailTxt] -> return $ T.lines $ T.strip detailTxt
          _              -> throw500 "splitOn DETAIL has unexpected structure"
        Nothing   -> throw500 "unexpected beginning of notice"
      let cascadeLines = mapMaybe (T.stripPrefix "drop cascades to") detailLines
      when (length detailLines /= length cascadeLines) $
        throw500 "unexpected lines in drop notice"
      return $ map T.strip cascadeLines

    parseCascadeLine cl
      | T.isPrefixOf "view" cl =
          case T.words cl of
            [_, vn] -> do
              qt <- dottedTxtToQualTable vn
              return $ Right $ SOTable qt
            _       -> throw500 $ "failed to parse view cascade line : " <> cl
      | T.isPrefixOf "constraint" cl =
          case T.words cl of
            [_, cn, _, _, tn] -> do
              qt <- dottedTxtToQualTable tn
              return $ Right $ SOTableObj qt $
                                 TOForeignKey $ ConstraintName cn
            _       -> throw500 $ "failed to parse constraint cascade line : " <> cl
      | otherwise = return $ Left cl

getPGDeps :: Q.Tx () -> Q.TxE QErr [Either T.Text SchemaObjId]
getPGDeps tx = do
  dropNotices <- Q.catchE defaultTxErrorHandler $ do
    Q.unitQ "SAVEPOINT hdb_get_pg_deps" () False
    dropNotices <- snd <$> Q.withNotices tx
    Q.unitQ "ROLLBACK TO SAVEPOINT hdb_get_pg_deps" () False
    Q.unitQ "RELEASE SAVEPOINT hdb_get_pg_deps" () False
    return dropNotices
  case dropNotices of
    []       -> return []
    [notice] -> parseDropNotice notice
    _        -> throw500 "unexpected number of notices when getting dependencies"

getIndirectDeps
  :: (CacheRM m, MonadTx m)
  => [SchemaObjId] -> Q.Tx ()
  -> m ([SchemaObjId], [T.Text])
getIndirectDeps initDeps tx = do
  sc <- askSchemaCache
  -- Now, trial run the drop sql to get pg dependencies
  pgDeps <- liftTx $ getPGDeps tx
  let (unparsedLines, parsedObjIds) = partitionEithers pgDeps
      indirectDeps = HS.fromList $ parsedObjIds <>
                     concatMap (getDependentObjs sc) parsedObjIds
      newDeps = indirectDeps `HS.difference` (HS.fromList initDeps)
  return (HS.toList newDeps, unparsedLines)
