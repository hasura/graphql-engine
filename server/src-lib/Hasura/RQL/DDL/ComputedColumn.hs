{- |
Description: Add/Drop computed columns in metadata
-}
{-# LANGUAGE QuasiQuotes #-}
module Hasura.RQL.DDL.ComputedColumn
  ( AddComputedColumn(..)
  , ComputedColumnDefinition(..)
  , runAddComputedColumn
  , addComputedColumnP2Setup
  , DropComputedColumn
  , dropComputedColumnFromCatalog
  , runDropComputedColumn
  ) where

import           Hasura.Prelude

import           Hasura.EncJSON
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.Permission.Internal
import           Hasura.RQL.DDL.Schema.Function     (RawFunctionInfo (..),
                                                     fetchRawFunctioInfo,
                                                     mkFunctionArgs)
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax         (Lift)

import qualified Control.Monad.Validate             as MV
import qualified Data.Sequence                      as Seq
import qualified Data.Text                          as T
import qualified Database.PG.Query                  as Q
import qualified Language.GraphQL.Draft.Syntax      as G

data ComputedColumnDefinition
  = ComputedColumnDefinition
  { _ccdFunction      :: !QualifiedFunction
  , _ccdTableArgument :: !(Maybe FunctionArgName)
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 4 snakeCase) ''ComputedColumnDefinition)

data AddComputedColumn
  = AddComputedColumn
  { _accTable      :: !QualifiedTable
  , _accName       :: !ComputedColumnName
  , _accDefinition :: !ComputedColumnDefinition
  , _accComment    :: !(Maybe Text)
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 4 snakeCase) ''AddComputedColumn)

runAddComputedColumn
  :: (QErrM m, CacheRWM m, MonadTx m , UserInfoM m)
  => AddComputedColumn -> m EncJSON
runAddComputedColumn q = do
  addComputedColumnP1 q
  addComputedColumnP2 q

addComputedColumnP1
  :: (UserInfoM m, QErrM m, CacheRM m)
  => AddComputedColumn -> m ()
addComputedColumnP1 q = do
  adminOnly
  tableInfo <- withPathK "table" $ askTabInfo tableName
  withPathK "name" $ checkForFieldConflict tableInfo $
    fromComputedColumn computedColumnName
  where
    AddComputedColumn tableName computedColumnName _ _ = q

addComputedColumnP2
  :: (QErrM m, CacheRWM m, MonadTx m)
  => AddComputedColumn -> m EncJSON
addComputedColumnP2 q = withPathK "definition" $ do
  rawFunctionInfo <- withPathK "function" $
    fetchRawFunctioInfo $ _ccdFunction definition
  addComputedColumnP2Setup table computedColumn definition rawFunctionInfo comment
  addComputedColumnToCatalog q
  return successMsg
  where
    AddComputedColumn table computedColumn definition comment = q

data ComputedColumnValidateError
  = CCVENotValidGraphQLName !ComputedColumnName
  | CCVEInvalidTableArgument !InvalidTableArgument
  | CCVENotBaseReturnType !PGScalarType
  | CCVEReturnTableNotFound !QualifiedTable
  | CCVENoInputArguments
  | CCVEFunctionVolatile
  deriving (Show, Eq)

data InvalidTableArgument
  = ITANotFound !FunctionArgName
  | ITANotComposite !FunctionTableArgument
  | ITANotTable !QualifiedTable !FunctionTableArgument
  deriving (Show, Eq)

showError :: QualifiedFunction -> ComputedColumnValidateError -> Text
showError qf = \case
  CCVENotValidGraphQLName computedColumn ->
    computedColumn <<> " is not valid GraphQL name"
  CCVEInvalidTableArgument (ITANotFound argName) ->
    argName <<> " is not an input argument of " <> qf <<> " function"
  CCVEInvalidTableArgument (ITANotComposite functionArg) ->
    showFunctionTableArgument functionArg <> " is not COMPOSITE type"
  CCVEInvalidTableArgument (ITANotTable ty functionArg) ->
    showFunctionTableArgument functionArg <> " of type " <> ty
    <<> " is not the table to which the computed column is being added"
  CCVENotBaseReturnType scalarType ->
    "the function " <> qf <<> " returning type " <> toSQLTxt scalarType
    <> " is not a BASE type"
  CCVEReturnTableNotFound table ->
    "the function " <> qf <<> " returning set of table " <> table
    <<> " is not tracked or not found in database"
  CCVENoInputArguments ->
    "the function " <> qf <<> " has no input arguments"
  CCVEFunctionVolatile ->
    "the function " <> qf <<> " is of type VOLATILE; cannot be added as a computed column"
  where
    showFunctionTableArgument = \case
      FTAFirstArgument -> "first argument of the function " <>> qf
      FTAName argName  -> argName <<> " argument of the function " <>> qf

addComputedColumnP2Setup
  :: (QErrM m, CacheRWM m)
  => QualifiedTable
  -> ComputedColumnName
  -> ComputedColumnDefinition
  -> RawFunctionInfo
  -> Maybe Text
  -> m ()
addComputedColumnP2Setup table computedColumn definition rawFunctionInfo comment = do
  sc <- askSchemaCache
  computedColumnInfo <- either (throw400 NotSupported . showErrors) pure
                        =<< MV.runValidateT (mkComputedColumnInfo sc)
  addComputedColumnToCache table computedColumnInfo
  where
    inputArgNames = rfiInputArgNames rawFunctionInfo
    ComputedColumnDefinition function maybeTableArg = definition
    functionReturnType = QualifiedPGType (rfiReturnTypeSchema rawFunctionInfo)
                         (rfiReturnTypeName rawFunctionInfo)
                         (rfiReturnTypeType rawFunctionInfo)

    computedColumnGraphQLName = G.Name $ computedColumnNameToText computedColumn

    mkComputedColumnInfo :: (MV.MonadValidate [ComputedColumnValidateError] m)
                          => SchemaCache -> m ComputedColumnInfo
    mkComputedColumnInfo sc = do
      -- Check if computed column name is a valid GraphQL name
      unless (G.isValidName computedColumnGraphQLName) $
        MV.dispute $ pure $ CCVENotValidGraphQLName computedColumn

      -- Check if function is VOLATILE
      when (rfiFunctionType rawFunctionInfo == FTVOLATILE) $
        MV.dispute $ pure CCVEFunctionVolatile

      -- Validate and resolve return type
      returnType <-
        if rfiReturnsTable rawFunctionInfo then do
          let returnTable = typeToTable functionReturnType
          unless (isTableTracked sc returnTable) $ MV.dispute $ pure $
            CCVEReturnTableNotFound returnTable
          pure $ CCRSetofTable returnTable
        else do
          let scalarType = _qptName functionReturnType
          unless (isBaseType functionReturnType) $ MV.dispute $ pure $
            CCVENotBaseReturnType scalarType
          pure $ CCRScalar scalarType

      -- Validate and resolve table argument
      let inputArgs = mkFunctionArgs (rfiDefaultArgs rawFunctionInfo)
                         (rfiInputArgTypes rawFunctionInfo) inputArgNames
      tableArgument <- case maybeTableArg of
        Just argName ->
          case find (maybe False (argName ==) . faName) inputArgs of
            Just tableArg -> do
              validateTableArgumentType (FTAName argName) $ faType tableArg
              pure $ FTAName argName
            Nothing ->
              MV.refute $ pure $ CCVEInvalidTableArgument $ ITANotFound argName
        Nothing -> do
          case inputArgs of
            []           -> MV.dispute $ pure CCVENoInputArguments
            (firstArg:_) ->
              validateTableArgumentType FTAFirstArgument $ faType firstArg
          pure FTAFirstArgument


      let functionArgs = Seq.fromList $ mkFunctionArgs (rfiDefaultArgs rawFunctionInfo)
                         (rfiInputArgTypes rawFunctionInfo) inputArgNames
          computedColumnFunction =
            ComputedColumnFunction function functionArgs tableArgument $
            rfiDescription rawFunctionInfo

      pure $ ComputedColumnInfo computedColumn computedColumnFunction returnType comment

    validateTableArgumentType :: (MV.MonadValidate [ComputedColumnValidateError] m)
                              => FunctionTableArgument
                              -> QualifiedPGType
                              -> m ()
    validateTableArgumentType tableArg qpt = do
      when (_qptType qpt /= PTCOMPOSITE) $
        MV.dispute $ pure $ CCVEInvalidTableArgument $ ITANotComposite tableArg
      let typeTable = typeToTable qpt
      unless (table == typeTable) $
        MV.dispute $ pure $ CCVEInvalidTableArgument $ ITANotTable typeTable tableArg

    showErrors :: [ComputedColumnValidateError] -> Text
    showErrors allErrors =
      "the computed column " <> computedColumn <<> " cannot be added to table "
      <> table <<> reasonMessage
      where
        reasonMessage = case allErrors of
          [singleError] -> " because " <> showError function singleError
          _ -> " for the following reasons: \n" <> T.unlines
               (map (("  • " <>) . showError function) allErrors)

addComputedColumnToCatalog
  :: MonadTx m
  => AddComputedColumn -> m ()
addComputedColumnToCatalog q =
  liftTx $ Q.withQE defaultTxErrorHandler
    [Q.sql|
     INSERT INTO hdb_catalog.hdb_computed_column
       (table_schema, table_name, computed_column_name, definition, comment)
     VALUES ($1, $2, $3, $4, $5)
    |] (schemaName, tableName, computedColumn, Q.AltJ definition, comment) True
  where
    QualifiedObject schemaName tableName = table
    AddComputedColumn table computedColumn definition comment = q

data DropComputedColumn
  = DropComputedColumn
  { _dccTable   :: !QualifiedTable
  , _dccName    :: !ComputedColumnName
  , _dccCascade :: !Bool
  } deriving (Show, Eq, Lift)
$(deriveToJSON (aesonDrop 4 snakeCase) ''DropComputedColumn)

instance FromJSON DropComputedColumn where
  parseJSON = withObject "Object" $ \o ->
    DropComputedColumn
      <$> o .: "table"
      <*> o .: "name"
      <*> o .:? "cascade" .!= False

runDropComputedColumn
  :: (UserInfoM m, CacheRWM m, MonadTx m)
  => DropComputedColumn -> m EncJSON
runDropComputedColumn (DropComputedColumn table computedColumn cascade) = do
  -- Validation
  adminOnly
  fields <- withPathK "table" $ _tiFieldInfoMap <$> askTabInfo table
  void $ withPathK "name" $ askComputedColumnInfo fields computedColumn

  -- Dependencies check
  sc <- askSchemaCache
  let deps = getDependentObjs sc $ SOTableObj table $ TOComputedColumn computedColumn
  when (not cascade && not (null deps)) $ reportDeps deps
  mapM_ purgeComputedColumnDependency deps

  deleteComputedColumnFromCache table computedColumn
  dropComputedColumnFromCatalog table computedColumn
  pure successMsg
  where
    purgeComputedColumnDependency = \case
      SOTableObj qt (TOPerm role permType) | qt == table -> do
        liftTx $ dropPermFromCatalog qt role permType
        withPermType permType delPermFromCache role qt
      d -> throw500 $ "unexpected dependency for computed column "
           <> computedColumn <<> "; " <> reportSchemaObj d

dropComputedColumnFromCatalog
  :: MonadTx m
  => QualifiedTable -> ComputedColumnName -> m ()
dropComputedColumnFromCatalog (QualifiedObject schema table) computedColumn =
  liftTx $ Q.withQE defaultTxErrorHandler
    [Q.sql|
     DELETE FROM hdb_catalog.hdb_computed_column
      WHERE table_schema = $1
        AND table_name = $2
        AND computed_column_name = $3
    |] (schema, table, computedColumn) True
