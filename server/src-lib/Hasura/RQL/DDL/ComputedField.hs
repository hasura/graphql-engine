{- |
Description: Add/Drop computed fields in metadata
-}
module Hasura.RQL.DDL.ComputedField
  ( AddComputedField(..)
  , ComputedFieldDefinition(..)
  , runAddComputedField
  , addComputedFieldP2
  , addComputedFieldP2Setup
  , DropComputedField
  , dropComputedFieldFromCatalog
  , runDropComputedField
  ) where

import           Hasura.Prelude

import           Hasura.EncJSON
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.Permission.Internal
import           Hasura.RQL.DDL.Schema.Function     (RawFunctionInfo (..),
                                                     fetchRawFunctioInfo,
                                                     mkFunctionArgs)
import           Hasura.RQL.Types
import           Hasura.Server.Utils                (makeReasonMessage)
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax         (Lift)

import qualified Control.Monad.Validate             as MV
import qualified Data.Sequence                      as Seq
import qualified Database.PG.Query                  as Q
import qualified Language.GraphQL.Draft.Syntax      as G

data ComputedFieldDefinition
  = ComputedFieldDefinition
  { _cfdFunction      :: !QualifiedFunction
  , _cfdTableArgument :: !(Maybe FunctionArgName)
  } deriving (Show, Eq, Lift, Generic)
$(deriveJSON (aesonDrop 4 snakeCase) ''ComputedFieldDefinition)

data AddComputedField
  = AddComputedField
  { _afcTable      :: !QualifiedTable
  , _afcName       :: !ComputedFieldName
  , _afcDefinition :: !ComputedFieldDefinition
  , _afcComment    :: !(Maybe Text)
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 4 snakeCase) ''AddComputedField)

runAddComputedField
  :: (QErrM m, CacheRWM m, MonadTx m , UserInfoM m)
  => AddComputedField -> m EncJSON
runAddComputedField q = do
  addComputedFieldP1 q
  addComputedFieldP2 q

addComputedFieldP1
  :: (UserInfoM m, QErrM m, CacheRM m)
  => AddComputedField -> m ()
addComputedFieldP1 q = do
  tableInfo <- withPathK "table" $ askTabInfo tableName
  withPathK "name" $ checkForFieldConflict tableInfo $
    fromComputedField computedFieldName
  where
    AddComputedField tableName computedFieldName _ _ = q

addComputedFieldP2
  :: (QErrM m, CacheRWM m, MonadTx m)
  => AddComputedField -> m EncJSON
addComputedFieldP2 q = withPathK "definition" $ do
  rawFunctionInfo <- withPathK "function" $
    fetchRawFunctioInfo $ _cfdFunction definition
  addComputedFieldP2Setup table computedField definition rawFunctionInfo comment
  addComputedFieldToCatalog q
  return successMsg
  where
    AddComputedField table computedField definition comment = q

data ComputedFieldValidateError
  = CFVENotValidGraphQLName !ComputedFieldName
  | CFVEInvalidTableArgument !InvalidTableArgument
  | CFVENotBaseReturnType !PGScalarType
  | CFVEReturnTableNotFound !QualifiedTable
  | CFVENoInputArguments
  | CFVEFunctionVolatile
  deriving (Show, Eq)

data InvalidTableArgument
  = ITANotFound !FunctionArgName
  | ITANotComposite !FunctionTableArgument
  | ITANotTable !QualifiedTable !FunctionTableArgument
  deriving (Show, Eq)

showError :: QualifiedFunction -> ComputedFieldValidateError -> Text
showError qf = \case
  CFVENotValidGraphQLName computedField ->
    computedField <<> " is not valid GraphQL name"
  CFVEInvalidTableArgument (ITANotFound argName) ->
    argName <<> " is not an input argument of " <> qf <<> " function"
  CFVEInvalidTableArgument (ITANotComposite functionArg) ->
    showFunctionTableArgument functionArg <> " is not COMPOSITE type"
  CFVEInvalidTableArgument (ITANotTable ty functionArg) ->
    showFunctionTableArgument functionArg <> " of type " <> ty
    <<> " is not the table to which the computed field is being added"
  CFVENotBaseReturnType scalarType ->
    "the function " <> qf <<> " returning type " <> toSQLTxt scalarType
    <> " is not a BASE type"
  CFVEReturnTableNotFound table ->
    "the function " <> qf <<> " returning set of table " <> table
    <<> " is not tracked or not found in database"
  CFVENoInputArguments ->
    "the function " <> qf <<> " has no input arguments"
  CFVEFunctionVolatile ->
    "the function " <> qf <<> " is of type VOLATILE; cannot be added as a computed field"
  where
    showFunctionTableArgument = \case
      FTAFirst          -> "first argument of the function " <>> qf
      FTANamed argName _ -> argName <<> " argument of the function " <>> qf

addComputedFieldP2Setup
  :: (QErrM m, CacheRWM m)
  => QualifiedTable
  -> ComputedFieldName
  -> ComputedFieldDefinition
  -> RawFunctionInfo
  -> Maybe Text
  -> m ()
addComputedFieldP2Setup table computedField definition rawFunctionInfo comment = do
  sc <- askSchemaCache
  computedFieldInfo <- either (throw400 NotSupported . showErrors) pure
                        =<< MV.runValidateT (mkComputedFieldInfo sc)
  addComputedFieldToCache table computedFieldInfo
  where
    inputArgNames = rfiInputArgNames rawFunctionInfo
    ComputedFieldDefinition function maybeTableArg = definition
    functionReturnType = QualifiedPGType (rfiReturnTypeSchema rawFunctionInfo)
                         (rfiReturnTypeName rawFunctionInfo)
                         (rfiReturnTypeType rawFunctionInfo)

    computedFieldGraphQLName = G.Name $ computedFieldNameToText computedField

    mkComputedFieldInfo :: (MV.MonadValidate [ComputedFieldValidateError] m)
                          => SchemaCache -> m ComputedFieldInfo
    mkComputedFieldInfo sc = do
      -- Check if computed field name is a valid GraphQL name
      unless (G.isValidName computedFieldGraphQLName) $
        MV.dispute $ pure $ CFVENotValidGraphQLName computedField

      -- Check if function is VOLATILE
      when (rfiFunctionType rawFunctionInfo == FTVOLATILE) $
        MV.dispute $ pure CFVEFunctionVolatile

      -- Validate and resolve return type
      returnType <-
        if rfiReturnsTable rawFunctionInfo then do
          let returnTable = typeToTable functionReturnType
          unless (isTableTracked sc returnTable) $ MV.dispute $ pure $
            CFVEReturnTableNotFound returnTable
          pure $ CFRSetofTable returnTable
        else do
          let scalarType = _qptName functionReturnType
          unless (isBaseType functionReturnType) $ MV.dispute $ pure $
            CFVENotBaseReturnType scalarType
          pure $ CFRScalar scalarType

      -- Validate and resolve table argument
      let inputArgs = mkFunctionArgs (rfiDefaultArgs rawFunctionInfo)
                         (rfiInputArgTypes rawFunctionInfo) inputArgNames
      tableArgument <- case maybeTableArg of
        Just argName ->
          case findWithIndex (maybe False (argName ==) . faName) inputArgs of
            Just (tableArg, index) -> do
              let functionTableArg = FTANamed argName index
              validateTableArgumentType functionTableArg $ faType tableArg
              pure functionTableArg
            Nothing ->
              MV.refute $ pure $ CFVEInvalidTableArgument $ ITANotFound argName
        Nothing -> do
          case inputArgs of
            []           -> MV.dispute $ pure CFVENoInputArguments
            (firstArg:_) ->
              validateTableArgumentType FTAFirst $ faType firstArg
          pure FTAFirst


      let inputArgSeq = Seq.fromList $ dropTableArgument tableArgument inputArgs
          computedFieldFunction =
            ComputedFieldFunction function inputArgSeq tableArgument $
            rfiDescription rawFunctionInfo

      pure $ ComputedFieldInfo computedField computedFieldFunction returnType comment

    validateTableArgumentType :: (MV.MonadValidate [ComputedFieldValidateError] m)
                              => FunctionTableArgument
                              -> QualifiedPGType
                              -> m ()
    validateTableArgumentType tableArg qpt = do
      when (_qptType qpt /= PGKindComposite) $
        MV.dispute $ pure $ CFVEInvalidTableArgument $ ITANotComposite tableArg
      let typeTable = typeToTable qpt
      unless (table == typeTable) $
        MV.dispute $ pure $ CFVEInvalidTableArgument $ ITANotTable typeTable tableArg

    showErrors :: [ComputedFieldValidateError] -> Text
    showErrors allErrors =
      "the computed field " <> computedField <<> " cannot be added to table "
      <> table <<> " " <> reasonMessage
      where
        reasonMessage = makeReasonMessage allErrors (showError function)

    dropTableArgument :: FunctionTableArgument -> [FunctionArg] -> [FunctionArg]
    dropTableArgument tableArg inputArgs =
      case tableArg of
        FTAFirst  -> tail inputArgs
        FTANamed argName _ ->
          filter ((/=) (Just argName) . faName) inputArgs

addComputedFieldToCatalog
  :: MonadTx m
  => AddComputedField -> m ()
addComputedFieldToCatalog q =
  liftTx $ Q.withQE defaultTxErrorHandler
    [Q.sql|
     INSERT INTO hdb_catalog.hdb_computed_field
       (table_schema, table_name, computed_field_name, definition, comment)
     VALUES ($1, $2, $3, $4, $5)
    |] (schemaName, tableName, computedField, Q.AltJ definition, comment) True
  where
    QualifiedObject schemaName tableName = table
    AddComputedField table computedField definition comment = q

data DropComputedField
  = DropComputedField
  { _dccTable   :: !QualifiedTable
  , _dccName    :: !ComputedFieldName
  , _dccCascade :: !Bool
  } deriving (Show, Eq, Lift)
$(deriveToJSON (aesonDrop 4 snakeCase) ''DropComputedField)

instance FromJSON DropComputedField where
  parseJSON = withObject "Object" $ \o ->
    DropComputedField
      <$> o .: "table"
      <*> o .: "name"
      <*> o .:? "cascade" .!= False

runDropComputedField
  :: (UserInfoM m, CacheRWM m, MonadTx m)
  => DropComputedField -> m EncJSON
runDropComputedField (DropComputedField table computedField cascade) = do
  -- Validation
  fields <- withPathK "table" $ _tiFieldInfoMap <$> askTabInfo table
  void $ withPathK "name" $ askComputedFieldInfo fields computedField

  -- Dependencies check
  sc <- askSchemaCache
  let deps = getDependentObjs sc $ SOTableObj table $ TOComputedField computedField
  when (not cascade && not (null deps)) $ reportDeps deps
  mapM_ purgeComputedFieldDependency deps

  deleteComputedFieldFromCache table computedField
  dropComputedFieldFromCatalog table computedField
  pure successMsg
  where
    purgeComputedFieldDependency = \case
      SOTableObj qt (TOPerm role permType) | qt == table -> do
        liftTx $ dropPermFromCatalog qt role permType
        withPermType permType delPermFromCache role qt
      d -> throw500 $ "unexpected dependency for computed field "
           <> computedField <<> "; " <> reportSchemaObj d

dropComputedFieldFromCatalog
  :: MonadTx m
  => QualifiedTable -> ComputedFieldName -> m ()
dropComputedFieldFromCatalog (QualifiedObject schema table) computedField =
  liftTx $ Q.withQE defaultTxErrorHandler
    [Q.sql|
     DELETE FROM hdb_catalog.hdb_computed_field
      WHERE table_schema = $1
        AND table_name = $2
        AND computed_field_name = $3
    |] (schema, table, computedField) True
