-- | Tools to analyze the structure of a GraphQL request.
module Hasura.GraphQL.Analyse
  ( -- * Query structure
    Structure (..),
    Selection (..),
    FieldInfo (..),
    VariableInfo (..),

    -- * Analysis
    diagnoseGraphQLQuery,
    analyzeGraphQLQuery,
  )
where

import Control.Monad.Writer (Writer, runWriter)
import Data.HashMap.Strict qualified as Map
import Data.Sequence ((|>))
import Data.Text qualified as T
import Hasura.GraphQL.Parser.Constants qualified as G
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G

-------------------------------------------------------------------------------
-- GraphQL query structure

-- | Overall structure of a given query.
data Structure = Structure
  { _stSelection :: Selection,
    _stVariables :: HashMap G.Name VariableInfo
  }

instance Semigroup Structure where
  Structure s1 v1 <> Structure s2 v2 = Structure (s1 <> s2) (v1 <> v2)

instance Monoid Structure where
  mempty = Structure mempty mempty

-- | Represents a selection of fields within a query.
data Selection = Selection
  { _seFields :: HashMap G.Name FieldInfo
  }

-- | In case of field collisions, we want to keep the union of all their
-- selections sets. For instance, given:
--
--  >  query MyQuery {
--  >    test {
--  >      a
--  >      b
--  >    }
--  >    test {
--  >      b
--  >      c
--  >    }
--  >  }
--
-- We want to keep a Selection with all three @a@, @b@, and @c@.
instance Semigroup Selection where
  Selection s1 <> Selection s2 = Selection $ Map.unionWith combineFields s1 s2
    where
      combineFields f1 f2 = f1 {_fiSelection = _fiSelection f1 <> _fiSelection f2}

instance Monoid Selection where
  mempty = Selection mempty

data FieldInfo = FieldInfo
  { _fiType :: G.GType,
    _fiTypeDefinition :: G.TypeDefinition [G.Name] G.InputValueDefinition,
    _fiSelection :: Maybe Selection
  }

-- | TODO: document this
data VariableInfo = VariableInfo
  { _viType :: G.GType,
    _viDefaultValue :: Maybe (G.Value Void)
  }

-------------------------------------------------------------------------------
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
analyzeGraphQLQuery ::
  G.SchemaIntrospection ->
  G.TypedOperationDefinition G.NoFragments G.Name ->
  (Structure, [Text])
analyzeGraphQLQuery schema G.TypedOperationDefinition {..} = runAnalysis schema $
  do
    -- traverse the fields recursively
    selection <- withCatchAndRecord do
      let rootTypeName = case _todType of
            G.OperationTypeQuery -> queryRootName
            G.OperationTypeMutation -> mutationRootName
            G.OperationTypeSubscription -> subscriptionRootName
      rootTypeDefinition <-
        lookupType rootTypeName
          `onNothingM` throwDiagnosis (TypeNotFound rootTypeName)
      analyzeSelectionSet rootTypeDefinition _todSelectionSet
        `onNothingM` throwDiagnosis NoTopLevelSelection
    -- collect variables
    let variables =
          _todVariableDefinitions <&> \G.VariableDefinition {..} ->
            (_vdName, VariableInfo _vdType _vdDefaultValue)
    pure $
      Structure
        (fromMaybe mempty selection)
        (Map.fromList variables)

-- | Check the consistency between the schema information about a type and a
-- selection set (or lack thereof) on that type.
analyzeSelectionSet ::
  G.TypeDefinition [G.Name] G.InputValueDefinition ->
  G.SelectionSet G.NoFragments G.Name ->
  Analysis (Maybe Selection)
analyzeSelectionSet typeDef selectionSet = case typeDef of
  G.TypeDefinitionInputObject iotd -> do
    -- input objects aren't allowed in selection sets
    throwDiagnosis $ InputObjectSelectionSet $ G._iotdName iotd
  G.TypeDefinitionScalar std -> do
    -- scalars do not admit a selection set
    unless (null selectionSet) $
      throwDiagnosis $ ScalarSelectionSet $ G._stdName std
    pure Nothing
  G.TypeDefinitionEnum etd -> do
    -- enums do not admit a selection set
    unless (null selectionSet) $
      throwDiagnosis $ EnumSelectionSet $ G._etdName etd
    pure Nothing
  G.TypeDefinitionUnion _utd ->
    -- TODO: implement unions
    pure Nothing
  G.TypeDefinitionInterface _itd ->
    -- TODO: implement interfaces
    pure Nothing
  G.TypeDefinitionObject otd ->
    Just <$> analyzeObjectSelectionSet otd selectionSet

-- | Analyze the fields of an object selection set against its definition, and
-- emit the corresponding 'Selection'. We ignore the fields that fail, and we
-- continue accumulating the others.
analyzeObjectSelectionSet ::
  G.ObjectTypeDefinition G.InputValueDefinition ->
  G.SelectionSet G.NoFragments G.Name ->
  Analysis Selection
analyzeObjectSelectionSet (G.ObjectTypeDefinition {..}) selectionSet = do
  mconcat . catMaybes <$> for selectionSet analyzeObjectSelection
  where
    analyzeObjectSelection :: G.Selection G.NoFragments G.Name -> Analysis (Maybe Selection)
    analyzeObjectSelection = \case
      G.SelectionInlineFragment inlineFrag -> do
        -- analyze the inline fragment's selection set
        mconcat <$> for (G._ifSelectionSet inlineFrag) analyzeObjectSelection
      G.SelectionField (G.Field {..}) ->
        withField _fName $ withCatchAndRecord do
          -- attempt to find that field in the object's definition
          G.FieldDefinition {..} <-
            findDefinition _fName
              `onNothing` throwDiagnosis (ObjectFieldNotFound _otdName _fName)
          -- attempt to find its type in the schema
          let baseType = G.getBaseType _fldType
          typeDefinition <-
            lookupType baseType
              `onNothingM` throwDiagnosis (TypeNotFound baseType)
          -- recursively processthe selection set
          subSelection <- analyzeSelectionSet typeDefinition _fSelectionSet
          -- TODO: should we check arguments?
          pure $
            Selection $
              Map.singleton
                (fromMaybe _fName _fAlias)
                (FieldInfo _fldType typeDefinition subSelection)
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

-------------------------------------------------------------------------------
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
      MonadError AnalysisError
    )

runAnalysis :: Monoid a => G.SchemaIntrospection -> Analysis a -> (a, [Text])
runAnalysis schema (Analysis a) =
  postProcess $
    runWriter $
      flip runReaderT (pure "$", schema) $
        runExceptT a
  where
    -- if there was an uncaught error, add it to the list
    postProcess = \case
      (Left err, errors) ->
        postProcess (Right mempty, errors ++ [err])
      (Right result, errors) ->
        (result, map render errors)

-- | Look up a type in the schema.
lookupType :: G.Name -> Analysis (Maybe (G.TypeDefinition [G.Name] G.InputValueDefinition))
lookupType name = do
  G.SchemaIntrospection types <- asks snd
  pure $ Map.lookup name types

-- | Add the current field to the error path.
withField :: G.Name -> Analysis a -> Analysis a
withField name = local $ first (|> G.unName name)

-- | Throws an 'AnalysisError' by combining the given diagnosis with the current
-- path. This interrupts the computation in the given branch, and must be caught
-- for the analysis to resume.
throwDiagnosis :: Diagnosis -> Analysis a
throwDiagnosis d = do
  currentPath <- asks fst
  throwError $ AnalysisError currentPath d

-- | Runs the given computation. if it fails, cacthes the error, records it in
-- the monad, and return 'Nothing'. This allows for a clean recovery.
withCatchAndRecord :: Analysis a -> Analysis (Maybe a)
withCatchAndRecord action =
  fmap Just action `catchError` \e -> do
    tell [e]
    pure Nothing

-------------------------------------------------------------------------------
-- Analysis errors

data AnalysisError = AnalysisError
  { _aePath :: Path,
    _aeDiagnosis :: Diagnosis
  }

type Path = Seq Text

data Diagnosis
  = NoTopLevelSelection
  | TypeNotFound G.Name
  | EnumSelectionSet G.Name
  | ScalarSelectionSet G.Name
  | InputObjectSelectionSet G.Name
  | ObjectFieldNotFound G.Name G.Name

render :: AnalysisError -> Text
render (AnalysisError path diagnosis) =
  T.intercalate "." (toList path) <> ": " <> case diagnosis of
    NoTopLevelSelection ->
      "did not find a valid selection set at the top level"
    TypeNotFound name ->
      "type '" <> G.unName name <> "' not found in the schema"
    EnumSelectionSet name ->
      "enum '" <> G.unName name <> "' does not accept a selection set"
    ScalarSelectionSet name ->
      "scalar '" <> G.unName name <> "' does not accept a selection set"
    InputObjectSelectionSet name ->
      "input object '" <> G.unName name <> "' cannot be used for output"
    ObjectFieldNotFound objName fieldName ->
      "field '" <> G.unName fieldName <> "' not found in object '" <> G.unName objName <> "'"

-------------------------------------------------------------------------------
-- GraphQL internals

-- Special type names

queryRootName :: G.Name
queryRootName = G._query_root

mutationRootName :: G.Name
mutationRootName = G._mutation_root

subscriptionRootName :: G.Name
subscriptionRootName = G._subscription_root

-- Reserved fields

typenameField :: G.FieldDefinition G.InputValueDefinition
typenameField = mkReservedField G.___typename G._String

schemaField :: G.FieldDefinition G.InputValueDefinition
schemaField = mkReservedField G.___schema G.___Schema

typeField :: G.FieldDefinition G.InputValueDefinition
typeField = mkReservedField G.___type G.___Type

mkReservedField :: G.Name -> G.Name -> G.FieldDefinition G.InputValueDefinition
mkReservedField fieldName typeName =
  G.FieldDefinition Nothing fieldName [] (G.TypeNamed (G.Nullability False) typeName) []
