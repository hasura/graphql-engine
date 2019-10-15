{- |
Description: Create/delete SQL functions to/from Hasura metadata.
-}

module Hasura.RQL.DDL.Schema.Function where

import           Hasura.EncJSON
import           Hasura.GraphQL.Utils          (showNames)
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax    (Lift)

import qualified Hasura.GraphQL.Schema         as GS
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Data.HashMap.Strict           as M
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q


data PGTypType
  = PTBASE
  | PTCOMPOSITE
  | PTDOMAIN
  | PTENUM
  | PTRANGE
  | PTPSEUDO
  deriving (Show, Eq)
$(deriveJSON defaultOptions{constructorTagModifier = drop 2} ''PGTypType)

data RawFuncInfo
  = RawFuncInfo
  { rfiHasVariadic      :: !Bool
  , rfiFunctionType     :: !FunctionType
  , rfiReturnTypeSchema :: !SchemaName
  , rfiReturnTypeName   :: !T.Text
  , rfiReturnTypeType   :: !PGTypType
  , rfiReturnsSet       :: !Bool
  , rfiInputArgTypes    :: ![PGScalarType]
  , rfiInputArgNames    :: ![T.Text]
  , rfiDefaultArgs      :: !Int
  , rfiReturnsTable     :: !Bool
  , rfiDescription      :: !(Maybe PGDescription)
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase) ''RawFuncInfo)

mkFunctionArgs :: Int -> [PGScalarType] -> [T.Text] -> [FunctionArg]
mkFunctionArgs defArgsNo tys argNames =
  bool withNames withNoNames $ null argNames
  where
    hasDefaultBoolSeq = replicate (length argNames - defArgsNo) False
                        -- only last arguments can have default expression
                        <> replicate defArgsNo True

    tysWithHasDefault = zip tys hasDefaultBoolSeq

    withNoNames = flip map tysWithHasDefault $
                  \(ty, hasDef) -> FunctionArg Nothing ty hasDef
    withNames = zipWith mkArg argNames tysWithHasDefault

    mkArg "" (ty, hasDef) = FunctionArg Nothing ty hasDef
    mkArg n  (ty, hasDef) = FunctionArg (Just $ FunctionArgName n) ty hasDef

validateFuncArgs :: MonadError QErr m => [FunctionArg] -> m ()
validateFuncArgs args =
  unless (null invalidArgs) $ throw400 NotSupported $
    "arguments: " <> showNames invalidArgs
    <> " are not in compliance with GraphQL spec"
  where
    funcArgsText = mapMaybe (fmap getFuncArgNameTxt . faName) args
    invalidArgs = filter (not . G.isValidName) $ map G.Name funcArgsText

mkFunctionInfo
  :: (QErrM m, HasSystemDefined m) => QualifiedFunction -> RawFuncInfo -> m FunctionInfo
mkFunctionInfo qf rawFuncInfo = do
  -- throw error if function has variadic arguments
  when hasVariadic $ throw400 NotSupported "function with \"VARIADIC\" parameters are not supported"
  -- throw error if return type is not composite type
  when (retTyTyp /= PTCOMPOSITE) $ throw400 NotSupported "function does not return a \"COMPOSITE\" type"
  -- throw error if function do not returns SETOF
  unless retSet $ throw400 NotSupported "function does not return a SETOF"
  -- throw error if return type is not a valid table
  unless returnsTab $ throw400 NotSupported "function does not return a SETOF table"
  -- throw error if function type is VOLATILE
  when (funTy == FTVOLATILE) $ throw400 NotSupported "function of type \"VOLATILE\" is not supported now"

  let funcArgs = mkFunctionArgs defArgsNo inpArgTyps inpArgNames
  validateFuncArgs funcArgs

  systemDefined <- askSystemDefined
  let funcArgsSeq = Seq.fromList funcArgs
      dep = SchemaDependency (SOTable retTable) DRTable
      retTable = QualifiedObject retSn (TableName retN)
  return $ FunctionInfo qf systemDefined funTy funcArgsSeq retTable [dep] descM
  where
    RawFuncInfo hasVariadic funTy retSn retN retTyTyp retSet
                inpArgTyps inpArgNames defArgsNo returnsTab descM
                = rawFuncInfo

saveFunctionToCatalog :: QualifiedFunction -> SystemDefined -> Q.TxE QErr ()
saveFunctionToCatalog (QualifiedObject sn fn) systemDefined =
  Q.unitQE defaultTxErrorHandler [Q.sql|
         INSERT INTO "hdb_catalog"."hdb_function" VALUES ($1, $2, $3)
                 |] (sn, fn, systemDefined) False

delFunctionFromCatalog :: QualifiedFunction -> Q.TxE QErr ()
delFunctionFromCatalog (QualifiedObject sn fn) =
  Q.unitQE defaultTxErrorHandler [Q.sql|
         DELETE FROM hdb_catalog.hdb_function
         WHERE function_schema = $1
           AND function_name = $2
         |] (sn, fn) False

newtype TrackFunction
  = TrackFunction
  { tfName :: QualifiedFunction}
  deriving (Show, Eq, FromJSON, ToJSON, Lift)

-- | Track function, Phase 1:
-- Validate function tracking operation. Fails if function is already being
-- tracked, or if a table with the same name is being tracked.
trackFunctionP1
  :: (CacheRM m, UserInfoM m, QErrM m) => TrackFunction -> m ()
trackFunctionP1 (TrackFunction qf) = do
  adminOnly
  rawSchemaCache <- askSchemaCache
  when (M.member qf $ scFunctions rawSchemaCache) $
    throw400 AlreadyTracked $ "function already tracked : " <>> qf
  let qt = fmap (TableName . getFunctionTxt) qf
  when (M.member qt $ scTables rawSchemaCache) $
    throw400 NotSupported $ "table with name " <> qf <<> " already exists"

trackFunctionP2Setup :: (QErrM m, CacheRWM m, HasSystemDefined m, MonadTx m)
                     => QualifiedFunction -> RawFuncInfo -> m ()
trackFunctionP2Setup qf rawfi = do
  fi <- mkFunctionInfo qf rawfi
  let retTable = fiReturnType fi
      err = err400 NotExists $ "table " <> retTable <<> " is not tracked"
  sc <- askSchemaCache
  void $ liftMaybe err $ M.lookup retTable $ scTables sc
  addFunctionToCache fi

trackFunctionP2 :: (QErrM m, CacheRWM m, HasSystemDefined m, MonadTx m)
                => QualifiedFunction -> m EncJSON
trackFunctionP2 qf = do
  sc <- askSchemaCache
  let defGCtx = scDefaultRemoteGCtx sc
      funcNameGQL = GS.qualObjectToName qf
  -- check function name is in compliance with GraphQL spec
  unless (G.isValidName funcNameGQL) $ throw400 NotSupported $
    "function name " <> qf <<> " is not in compliance with GraphQL spec"
  -- check for conflicts in remote schema
  GS.checkConflictingNode defGCtx funcNameGQL

  -- fetch function info
  functionInfos <- liftTx fetchFuncDets
  rawfi <- case functionInfos of
    []      ->
      throw400 NotExists $ "no such function exists in postgres : " <>> qf
    [rawfi] -> return rawfi
    _       ->
      throw400 NotSupported $
      "function " <> qf <<> " is overloaded. Overloaded functions are not supported"
  trackFunctionP2Setup qf rawfi
  systemDefined <- askSystemDefined
  liftTx $ saveFunctionToCatalog qf systemDefined
  return successMsg
  where
    QualifiedObject sn fn = qf
    fetchFuncDets = map (Q.getAltJ . runIdentity) <$>
      Q.listQE defaultTxErrorHandler [Q.sql|
            SELECT function_info
              FROM hdb_catalog.hdb_function_info_agg
             WHERE function_schema = $1
               AND function_name = $2
           |] (sn, fn) True

runTrackFunc
  :: ( QErrM m, CacheRWM m, HasSystemDefined m
     , MonadTx m, UserInfoM m
     )
  => TrackFunction -> m EncJSON
runTrackFunc q = do
  trackFunctionP1 q
  trackFunctionP2 $ tfName q

newtype UnTrackFunction
  = UnTrackFunction
  { utfName :: QualifiedFunction }
  deriving (Show, Eq, FromJSON, ToJSON, Lift)

runUntrackFunc
  :: ( QErrM m, CacheRWM m, MonadTx m
     , UserInfoM m
     )
  => UnTrackFunction -> m EncJSON
runUntrackFunc (UnTrackFunction qf) = do
  adminOnly
  void $ askFunctionInfo qf
  liftTx $ delFunctionFromCatalog qf
  delFunctionFromCache qf
  return successMsg
