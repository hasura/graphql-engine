{- |
Description: Add/Drop computed fields in metadata
-}
module Hasura.RQL.DDL.ComputedField
  ( AddComputedField(..)
  , ComputedFieldDefinition(..)
  , runAddComputedField
  , addComputedFieldP2Setup
  , addComputedFieldToCatalog
  , DropComputedField
  , dropComputedFieldFromCatalog
  , runDropComputedField
  ) where

import           Hasura.Prelude

import qualified Control.Monad.Validate             as MV
import qualified Data.HashSet                       as S
import qualified Data.Sequence                      as Seq
import qualified Database.PG.Query                  as Q
import qualified Language.GraphQL.Draft.Syntax      as G

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Text.Extended

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.EncJSON
import           Hasura.Incremental                 (Cacheable)
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.Permission.Internal
import           Hasura.RQL.DDL.Schema.Function     (mkFunctionArgs)
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.Server.Utils                (makeReasonMessage)


data AddComputedField
  = AddComputedField
  { _afcTable      :: !QualifiedTable
  , _afcName       :: !ComputedFieldName
  , _afcDefinition :: !ComputedFieldDefinition
  , _afcComment    :: !(Maybe Text)
  } deriving (Show, Eq, Generic)
instance NFData AddComputedField
instance Cacheable AddComputedField
$(deriveJSON (aesonDrop 4 snakeCase) ''AddComputedField)

runAddComputedField :: (MonadTx m, CacheRWM m) => AddComputedField -> m EncJSON
runAddComputedField q = do
  withPathK "table" $ askTabInfo (_afcTable q)
  addComputedFieldToCatalog q
  buildSchemaCacheFor $ MOTableObj (_afcTable q) (MTOComputedField $ _afcName q)
  pure successMsg

data ComputedFieldValidateError
  = CFVENotValidGraphQLName !ComputedFieldName
  | CFVEInvalidTableArgument !InvalidTableArgument
  | CFVEInvalidSessionArgument !InvalidSessionArgument
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

data InvalidSessionArgument
  = ISANotFound !FunctionArgName
  | ISANotJSON !FunctionSessionArgument
  deriving (Show, Eq)

showError :: QualifiedFunction -> ComputedFieldValidateError -> Text
showError qf = \case
  CFVENotValidGraphQLName computedField ->
    computedField <<> " is not valid GraphQL name"
  CFVEInvalidTableArgument (ITANotFound argName) ->
    argName <<> " is not an input argument of the function " <>> qf
  CFVEInvalidTableArgument (ITANotComposite functionArg) ->
    showFunctionTableArgument functionArg <> " is not COMPOSITE type"
  CFVEInvalidTableArgument (ITANotTable ty functionArg) ->
    showFunctionTableArgument functionArg <> " of type " <> ty
    <<> " is not the table to which the computed field is being added"
  CFVEInvalidSessionArgument (ISANotFound argName) ->
    argName <<> " is not an input argument of the function " <>> qf
  CFVEInvalidSessionArgument (ISANotJSON functionArg) ->
    showFunctionSessionArgument functionArg <> " is not of type JSON"
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
      FTAFirst           -> "first argument of the function " <>> qf
      FTANamed argName _ -> argName <<> " argument of the function " <>> qf
    showFunctionSessionArgument = \case
      FunctionSessionArgument argName _ -> argName <<> " argument of the function " <>> qf

addComputedFieldP2Setup
  :: (QErrM m)
  => S.HashSet QualifiedTable
  -- ^ the set of all tracked tables
  -> QualifiedTable
  -> ComputedFieldName
  -> ComputedFieldDefinition
  -> RawFunctionInfo
  -> Maybe Text
  -> m (ComputedFieldInfo 'Postgres)
addComputedFieldP2Setup trackedTables table computedField definition rawFunctionInfo comment =
  either (throw400 NotSupported . showErrors) pure =<< MV.runValidateT mkComputedFieldInfo
  where
    inputArgNames = rfiInputArgNames rawFunctionInfo
    ComputedFieldDefinition function maybeTableArg maybeSessionArg = definition
    functionReturnType = QualifiedPGType (rfiReturnTypeSchema rawFunctionInfo)
                         (rfiReturnTypeName rawFunctionInfo)
                         (rfiReturnTypeType rawFunctionInfo)

    computedFieldGraphQLName = G.mkName $ computedFieldNameToText computedField

    mkComputedFieldInfo :: (MV.MonadValidate [ComputedFieldValidateError] m)
                          => m (ComputedFieldInfo 'Postgres)
    mkComputedFieldInfo = do
      -- Check if computed field name is a valid GraphQL name
      unless (isJust computedFieldGraphQLName) $
        MV.dispute $ pure $ CFVENotValidGraphQLName computedField

      -- Check if function is VOLATILE
      when (rfiFunctionType rawFunctionInfo == FTVOLATILE) $
        MV.dispute $ pure CFVEFunctionVolatile

      -- Validate and resolve return type
      returnType <-
        if rfiReturnsTable rawFunctionInfo then do
          let returnTable = typeToTable functionReturnType
          unless (returnTable `S.member` trackedTables) $ MV.dispute $ pure $
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
          case findWithIndex ((Just argName ==) . faName) inputArgs of
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

      maybePGSessionArg <- sequence $ do
          argName <- maybeSessionArg
          return $ case findWithIndex ((Just argName ==) . faName) inputArgs of
            Just (sessionArg, index) -> do
              let functionSessionArg = FunctionSessionArgument argName index
              validateSessionArgumentType functionSessionArg $ faType sessionArg
              pure functionSessionArg
            Nothing ->
              MV.refute $ pure $ CFVEInvalidSessionArgument $ ISANotFound argName


      let inputArgSeq = Seq.fromList $ dropTableAndSessionArgument tableArgument
                        maybePGSessionArg inputArgs
          computedFieldFunction =
            ComputedFieldFunction function inputArgSeq tableArgument maybePGSessionArg $
            rfiDescription rawFunctionInfo

      pure $ ComputedFieldInfo () computedField computedFieldFunction returnType comment

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

    validateSessionArgumentType :: (MV.MonadValidate [ComputedFieldValidateError] m)
                                => FunctionSessionArgument
                                -> QualifiedPGType
                                -> m ()
    validateSessionArgumentType sessionArg qpt = do
      unless (isJSONType $ _qptName qpt) $
        MV.dispute $ pure $ CFVEInvalidSessionArgument $ ISANotJSON sessionArg

    showErrors :: [ComputedFieldValidateError] -> Text
    showErrors allErrors =
      "the computed field " <> computedField <<> " cannot be added to table "
      <> table <<> " " <> reasonMessage
      where
        reasonMessage = makeReasonMessage allErrors (showError function)

    dropTableAndSessionArgument :: FunctionTableArgument
                                -> Maybe FunctionSessionArgument -> [FunctionArg]
                                -> [FunctionArg]
    dropTableAndSessionArgument tableArg sessionArg inputArgs =
      let withoutTable = case tableArg of
            FTAFirst  -> tail inputArgs
            FTANamed argName _ ->
              filter ((/=) (Just argName) . faName) inputArgs
          alsoWithoutSession = case sessionArg of
            Nothing -> withoutTable
            Just (FunctionSessionArgument name _) ->
              filter ((/=) (Just name) . faName) withoutTable
      in alsoWithoutSession


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
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 4 snakeCase) ''DropComputedField)

instance FromJSON DropComputedField where
  parseJSON = withObject "Object" $ \o ->
    DropComputedField
      <$> o .: "table"
      <*> o .: "name"
      <*> o .:? "cascade" .!= False

runDropComputedField
  :: (MonadTx m, CacheRWM m)
  => DropComputedField -> m EncJSON
runDropComputedField (DropComputedField table computedField cascade) = do
  -- Validation
  fields <- withPathK "table" $ _tciFieldInfoMap <$> askTableCoreInfo table
  void $ withPathK "name" $ askComputedFieldInfo fields computedField

  -- Dependencies check
  sc <- askSchemaCache
  let deps = getDependentObjs sc $ SOTableObj table $ TOComputedField computedField
  when (not cascade && not (null deps)) $ reportDeps deps

  withNewInconsistentObjsCheck do
    mapM_ purgeComputedFieldDependency deps
    dropComputedFieldFromCatalog table computedField
    buildSchemaCache
  pure successMsg
  where
    purgeComputedFieldDependency = \case
      SOTableObj qt (TOPerm roleName permType) | qt == table ->
        liftTx $ dropPermFromCatalog qt roleName permType
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
