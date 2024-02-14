-- | Tools to analyze the structure of a GraphQL request.
module Hasura.GraphQL.Analyse
  ( -- * Query structure
    Structure (..),
    FieldInfo (..),
    InputFieldInfo (..),
    VariableInfo (..),
    ScalarInfo (..),
    EnumInfo (..),
    ObjectInfo (..),
    InputObjectInfo (..),

    -- * Analysis
    diagnoseGraphQLQuery,
    analyzeGraphQLQuery,
  )
where

import Control.Monad.Circular
import Control.Monad.Writer (Writer, runWriter)
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.Sequence ((|>))
import Data.Text qualified as T
import Hasura.GraphQL.Parser.Name qualified as GName
import Hasura.Name qualified as Name
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G

--------------------------------------------------------------------------------
-- GraphQL query structure

-- | Overall structure of a given query. We extract the tree of fields in the
-- output, and the graph of input variables.
data Structure = Structure
  { _stSelection :: HashMap G.Name FieldInfo,
    _stVariables :: HashMap G.Name VariableInfo
  }

-- | Information about the type of an output field; whether the base type is an
-- object or a scalar, we store the correspoding 'GType' to keep track of the
-- modifiers applied to it (list or non-nullability).
data FieldInfo
  = FieldObjectInfo G.GType ObjectInfo
  | FieldScalarInfo G.GType ScalarInfo
  | FieldEnumInfo G.GType EnumInfo

data ScalarInfo = ScalarInfo
  { _siTypeDefinition :: G.ScalarTypeDefinition
  }

data EnumInfo = EnumInfo
  { _eiTypeDefinition :: G.EnumTypeDefinition
  }

data ObjectInfo = ObjectInfo
  { _oiTypeDefinition :: G.ObjectTypeDefinition G.InputValueDefinition,
    _oiSelection :: HashMap G.Name FieldInfo
  }

-- | Information about a single variable of the query.
data VariableInfo = VariableInfo
  { _viType :: G.GType,
    _viTypeInfo :: InputFieldInfo,
    _viDefaultValue :: Maybe (G.Value Void)
  }

-- | Information about the type of an input field; whether the base type is an
-- object or a scalar, we store the correspoding 'GType' to keep track of the
-- modifiers applied to it (list or non-nullability).
data InputFieldInfo
  = InputFieldScalarInfo ScalarInfo
  | InputFieldEnumInfo EnumInfo
  | InputFieldObjectInfo InputObjectInfo

data InputObjectInfo = InputObjectInfo
  { _ioiTypeDefinition :: G.InputObjectTypeDefinition G.InputValueDefinition,
    -- | lazy for knot-tying, as we build a graph
    _ioiFields :: ~(HashMap G.Name (G.GType, InputFieldInfo))
  }

--------------------------------------------------------------------------------
-- Analysis

-- | Given the schema's definition, and a query, validate that the query is
-- consistent. We do this by running the analysis, but discarding the result: we
-- do not care about the structure, only about the validity of the query.
--
-- Returns 'Nothing' if the query is valid, or a list of messages otherwise.
diagnoseGraphQLQuery ::
  G.SchemaIntrospection ->
  G.TypedOperationDefinition G.NoFragments G.Name ->
  Maybe [Text]
diagnoseGraphQLQuery schema query =
  let (_structure, errors) = analyzeGraphQLQuery schema query
   in if null errors
        then Nothing
        else Just errors

-- | Given the schema's definition, and a query, run the analysis.
--
-- We process all possible fields, and return a partially filled structure if
-- necessary. Given the following query:
--
--   > query {
--   >   foo {
--   >     bar
--   >   }
--   >   does_not_exist {
--   >     ghsdflgh
--   >   }
--   > }
--
-- We would return a structure containing:
--
--   > foo: {
--   >   bar: {
--   >   }
--   > }
--
-- AND an error about "does_not_exist" not existing.
--
-- In some cases, however, we might not be able to produce a structure at all,
-- in which case we return 'Nothing'. This either indicates that something was
-- fundamentally wrong with the structure of the query (such as not finding an
-- object at the top level), or that a recoverable error was not caught properly
-- (see 'withCatchAndRecord').
analyzeGraphQLQuery ::
  G.SchemaIntrospection ->
  G.TypedOperationDefinition G.NoFragments G.Name ->
  (Maybe Structure, [Text])
analyzeGraphQLQuery schema G.TypedOperationDefinition {..} = runAnalysis schema do
  -- analyze the selection
  selection <- withCatchAndRecord do
    let rootTypeName = case _todType of
          G.OperationTypeQuery -> queryRootName
          G.OperationTypeMutation -> mutationRootName
          G.OperationTypeSubscription -> subscriptionRootName
    rootTypeDefinition <-
      lookupType rootTypeName
        `onNothingM` throwDiagnosis (TypeNotFound rootTypeName)
    case rootTypeDefinition of
      G.TypeDefinitionObject otd ->
        analyzeObjectSelectionSet otd _todSelectionSet
      _ ->
        throwDiagnosis RootTypeNotAnObject
  -- analyze the variables
  variables <- analyzeVariables _todVariableDefinitions
  pure
    $ Structure
      (fromMaybe mempty selection)
      variables

--------------------------------------------------------------------------------
-- Selection analysis

-- | Analyze the fields of an object selection set against its definition, and
-- emit the corresponding 'Selection'. We ignore the fields that fail, and we
-- continue accumulating the others.
analyzeObjectSelectionSet ::
  G.ObjectTypeDefinition G.InputValueDefinition ->
  G.SelectionSet G.NoFragments G.Name ->
  Analysis (HashMap G.Name FieldInfo)
analyzeObjectSelectionSet (G.ObjectTypeDefinition {..}) selectionSet = do
  fields <- traverse analyzeSelection selectionSet
  foldlM (HashMap.unionWithM mergeFields) mempty $ catMaybes fields
  where
    analyzeSelection :: G.Selection G.NoFragments G.Name -> Analysis (Maybe (HashMap G.Name FieldInfo))
    analyzeSelection = \case
      G.SelectionInlineFragment inlineFrag ->
        mconcat <$> traverse analyzeSelection (G._ifSelectionSet inlineFrag)
      G.SelectionField field@G.Field {..} ->
        fmap join
          $ withField _fName
          $ withCatchAndRecord do
            -- attempt to find that field in the object's definition
            G.FieldDefinition {..} <-
              findDefinition _fName
                `onNothing` throwDiagnosis (ObjectFieldNotFound _otdName _fName)
            -- attempt to find its type in the schema
            let baseType = G.getBaseType _fldType
            typeDefinition <-
              lookupType baseType
                `onNothingM` throwDiagnosis (TypeNotFound baseType)
            -- attempt to build a corresponding FieldInfo
            maybeFieldInfo <- analyzeField _fldType typeDefinition field
            pure $ HashMap.singleton (fromMaybe _fName _fAlias) <$> maybeFieldInfo

    -- Additional hidden fields that are allowed despite not being listed in the
    -- schema.
    systemFields :: [G.FieldDefinition G.InputValueDefinition]
    systemFields =
      if _otdName `elem` [queryRootName, mutationRootName, subscriptionRootName]
        then [typenameField, schemaField, typeField]
        else [typenameField]

    -- Search for that field in the schema's definition.
    findDefinition :: G.Name -> Maybe (G.FieldDefinition G.InputValueDefinition)
    findDefinition name =
      find
        (\fieldDef -> G._fldName fieldDef == name)
        (_otdFieldsDefinition <> systemFields)

    -- We collect fields in a @Hashmap Name FieldInfo@; in some cases, we might
    -- end up with two entries with the same name, in the case where a query
    -- selects the same field twice; when that happens we attempt to gracefully
    -- merge the info.
    mergeFields :: G.Name -> FieldInfo -> FieldInfo -> Analysis FieldInfo
    mergeFields name field1 field2 = case (field1, field2) of
      -- both are scalars: we check that they're the same
      (FieldScalarInfo t1 s1, FieldScalarInfo t2 _) -> do
        when (t1 /= t2)
          $ throwDiagnosis
          $ MismatchedFields name t1 t2
        pure $ FieldScalarInfo t1 s1
      -- both are enums: we check that they're the same
      (FieldEnumInfo t1 e1, FieldEnumInfo t2 _) -> do
        when (t1 /= t2)
          $ throwDiagnosis
          $ MismatchedFields name t1 t2
        pure $ FieldEnumInfo t1 e1
      -- both are objects, we merge their selection sets
      (FieldObjectInfo t1 o1, FieldObjectInfo t2 o2) -> do
        when (t1 /= t2)
          $ throwDiagnosis
          $ MismatchedFields name t1 t2
        mergedSelection <-
          HashMap.unionWithM
            mergeFields
            (_oiSelection o1)
            (_oiSelection o2)
        pure $ FieldObjectInfo t1 o1 {_oiSelection = mergedSelection}
      -- they do not match
      _ ->
        throwDiagnosis $ MismatchedFields name (getFieldType field1) (getFieldType field2)

    -- Extract the GType of a given field
    getFieldType = \case
      FieldEnumInfo t _ -> t
      FieldScalarInfo t _ -> t
      FieldObjectInfo t _ -> t

-- | Analyze a given field, and attempt to build a corresponding 'FieldInfo'.
analyzeField ::
  G.GType ->
  G.TypeDefinition [G.Name] G.InputValueDefinition ->
  G.Field G.NoFragments G.Name ->
  Analysis (Maybe FieldInfo)
analyzeField gType typeDefinition G.Field {..} = case typeDefinition of
  G.TypeDefinitionInputObject iotd -> do
    -- input objects aren't allowed in selection sets
    throwDiagnosis $ InputObjectInOutput $ G._iotdName iotd
  G.TypeDefinitionScalar std -> do
    -- scalars do not admit a selection set
    unless (null _fSelectionSet)
      $ throwDiagnosis
      $ ScalarSelectionSet
      $ G._stdName std
    pure $ Just $ FieldScalarInfo gType $ ScalarInfo std
  G.TypeDefinitionEnum etd -> do
    -- enums do not admit a selection set
    unless (null _fSelectionSet)
      $ throwDiagnosis
      $ EnumSelectionSet
      $ G._etdName etd
    pure $ Just $ FieldEnumInfo gType $ EnumInfo etd
  G.TypeDefinitionUnion _utd ->
    -- TODO: implement unions
    pure Nothing
  G.TypeDefinitionInterface _itd ->
    -- TODO: implement interfaces
    pure Nothing
  G.TypeDefinitionObject otd -> do
    -- TODO: check field arguments?
    when (null _fSelectionSet)
      $ throwDiagnosis
      $ ObjectMissingSelectionSet
      $ G._otdName otd
    subselection <- analyzeObjectSelectionSet otd _fSelectionSet
    pure
      $ Just
      $ FieldObjectInfo gType
      $ ObjectInfo
        { _oiTypeDefinition = otd,
          _oiSelection = subselection
        }

--------------------------------------------------------------------------------
-- Variables analysis

-- | Analyzes the variables in the given query. This builds the graph of input
-- types associated with the variable. This process is, like any GraphQL schema
-- operation, inherently self-recursive, and we use 'CircularT' (a lesser
-- 'SchemaT') to tie the knot.
analyzeVariables ::
  [G.VariableDefinition] ->
  Analysis (HashMap G.Name VariableInfo)
analyzeVariables variables = do
  result <- runCircularT $ for variables \G.VariableDefinition {..} -> do
    -- TODO: do we want to differentiate field from variable in the error path?
    withField _vdName $ withCatchAndRecord do
      let baseType = G.getBaseType _vdType
      typeDefinition <-
        lookupType baseType
          `onNothingM` throwDiagnosis (TypeNotFound baseType)
      ifInfo <- analyzeInputField baseType typeDefinition
      pure $ HashMap.singleton _vdName $ VariableInfo _vdType ifInfo _vdDefaultValue
  pure $ fold $ catMaybes result

-- | Builds an 'InputFieldInfo' for a given typename.
--
-- This function is "memoized" using 'withCircular' to prevent processing the
-- same type more than once in case the input types are self-recursive.
analyzeInputField ::
  G.Name ->
  G.TypeDefinition [G.Name] G.InputValueDefinition ->
  CircularT G.Name InputFieldInfo Analysis InputFieldInfo
analyzeInputField typeName typeDefinition =
  withCircular typeName $ case typeDefinition of
    G.TypeDefinitionScalar std ->
      pure $ InputFieldScalarInfo (ScalarInfo std)
    G.TypeDefinitionEnum etd ->
      pure $ InputFieldEnumInfo (EnumInfo etd)
    G.TypeDefinitionInputObject iotd -> do
      fields <- for (G._iotdValueDefinitions iotd) \G.InputValueDefinition {..} -> do
        withField _ivdName $ withCatchAndRecord do
          let baseType = G.getBaseType _ivdType
          typeDef <-
            lookupType baseType
              `onNothingM` throwDiagnosis (TypeNotFound baseType)
          info <- analyzeInputField baseType typeDef
          pure (_ivdName, (_ivdType, info))
      pure $ InputFieldObjectInfo (InputObjectInfo iotd $ HashMap.fromList $ catMaybes fields)
    G.TypeDefinitionObject _otd -> throwDiagnosis $ ObjectInInput typeName
    G.TypeDefinitionInterface _itd -> throwDiagnosis $ InterfaceInInput typeName
    G.TypeDefinitionUnion _utd -> throwDiagnosis $ UnionInInput typeName

--------------------------------------------------------------------------------
-- Internal Analysis monad and helpers

-- | The monad in which we run our analysis.
--
-- Has three capabilities:
--   - reader carries the current path, and the full schema for lookups
--   - writer logs all errors we have caught
--   - except allows for short-circuiting errors
newtype Analysis a
  = Analysis
      ( ExceptT
          AnalysisError
          ( ReaderT
              (Path, G.SchemaIntrospection)
              (Writer [AnalysisError])
          )
          a
      )
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (Path, G.SchemaIntrospection),
      MonadWriter [AnalysisError],
      MonadError AnalysisError,
      MonadFix
    )

runAnalysis :: G.SchemaIntrospection -> Analysis a -> (Maybe a, [Text])
runAnalysis schema (Analysis a) =
  postProcess
    $ runWriter
    $ flip runReaderT (pure "$", schema)
    $ runExceptT a
  where
    -- if there was an uncaught error, add it to the list
    postProcess = \case
      (Left err, errors) ->
        (Nothing, map render $ errors ++ [err])
      (Right result, errors) ->
        (Just result, map render errors)

-- | Look up a type in the schema.
lookupType ::
  (MonadReader (Path, G.SchemaIntrospection) m) =>
  G.Name ->
  m (Maybe (G.TypeDefinition [G.Name] G.InputValueDefinition))
lookupType name = do
  G.SchemaIntrospection types <- asks snd
  pure $ HashMap.lookup name types

-- | Add the current field to the error path.
withField ::
  (MonadReader (Path, G.SchemaIntrospection) m) =>
  G.Name ->
  m a ->
  m a
withField name = local $ first (|> G.unName name)

-- | Throws an 'AnalysisError' by combining the given diagnosis with the current
-- path. This interrupts the computation in the given branch, and must be caught
-- for the analysis to resume.
throwDiagnosis ::
  ( MonadReader (Path, G.SchemaIntrospection) m,
    MonadError AnalysisError m
  ) =>
  Diagnosis ->
  m a
throwDiagnosis d = do
  currentPath <- asks fst
  throwError $ AnalysisError currentPath d

-- | Runs the given computation. if it fails, cacthes the error, records it in
-- the monad, and return 'Nothing'. This allows for a clean recovery.
withCatchAndRecord ::
  ( MonadReader (Path, G.SchemaIntrospection) m,
    MonadWriter [AnalysisError] m,
    MonadError AnalysisError m
  ) =>
  m a ->
  m (Maybe a)
withCatchAndRecord action =
  fmap Just action `catchError` \e -> do
    tell [e]
    pure Nothing

--------------------------------------------------------------------------------
-- Analysis errors

data AnalysisError = AnalysisError
  { _aePath :: Path,
    _aeDiagnosis :: Diagnosis
  }

type Path = Seq Text

data Diagnosis
  = RootTypeNotAnObject
  | TypeNotFound G.Name
  | EnumSelectionSet G.Name
  | ScalarSelectionSet G.Name
  | InputObjectInOutput G.Name
  | UnionInInput G.Name
  | ObjectInInput G.Name
  | InterfaceInInput G.Name
  | ObjectFieldNotFound G.Name G.Name
  | ObjectMissingSelectionSet G.Name
  | MismatchedFields G.Name G.GType G.GType

render :: AnalysisError -> Text
render (AnalysisError path diagnosis) =
  T.intercalate "." (toList path) <> ": " <> case diagnosis of
    RootTypeNotAnObject ->
      "the root type is not an object"
    TypeNotFound name ->
      "type '" <> G.unName name <> "' not found in the schema"
    EnumSelectionSet name ->
      "enum '" <> G.unName name <> "' does not accept a selection set"
    ScalarSelectionSet name ->
      "scalar '" <> G.unName name <> "' does not accept a selection set"
    InputObjectInOutput name ->
      "input object '" <> G.unName name <> "' cannot be used for output"
    UnionInInput name ->
      "union '" <> G.unName name <> "' cannot be used in an input type"
    ObjectInInput name ->
      "object '" <> G.unName name <> "' cannot be used in an input type"
    InterfaceInInput name ->
      "interface '" <> G.unName name <> "' cannot be used in an input type"
    ObjectFieldNotFound objName fieldName ->
      "field '" <> G.unName fieldName <> "' not found in object '" <> G.unName objName <> "'"
    ObjectMissingSelectionSet objName ->
      "object of type '" <> G.unName objName <> "' must have a selection set"
    MismatchedFields name type1 type2 ->
      "field '" <> G.unName name <> "' seen with two different types: " <> tshow type1 <> " and " <> tshow type2

--------------------------------------------------------------------------------
-- GraphQL internals

-- Special type names

queryRootName :: G.Name
queryRootName = Name._query_root

mutationRootName :: G.Name
mutationRootName = Name._mutation_root

subscriptionRootName :: G.Name
subscriptionRootName = Name._subscription_root

-- Reserved fields

typenameField :: G.FieldDefinition G.InputValueDefinition
typenameField = mkReservedField GName.___typename GName._String

schemaField :: G.FieldDefinition G.InputValueDefinition
schemaField = mkReservedField GName.___schema GName.___Schema

typeField :: G.FieldDefinition G.InputValueDefinition
typeField = mkReservedField GName.___type GName.___Type

mkReservedField :: G.Name -> G.Name -> G.FieldDefinition G.InputValueDefinition
mkReservedField fieldName typeName =
  G.FieldDefinition Nothing fieldName [] (G.TypeNamed (G.Nullability False) typeName) []
