module Hasura.GraphQL.Analyse
  ( Analysis (..),
    FieldAnalysis (..),
    FieldDef (..),
    analyzeGraphqlQuery,
    getAllAnalysisErrs,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Hasura.Prelude hiding (get, put)
import Hasura.RQL.Types.RemoteSchema
  ( RemoteSchemaInputValueDefinition (RemoteSchemaInputValueDefinition, _rsitdDefinition),
    RemoteSchemaIntrospection (RemoteSchemaIntrospection),
  )
import Language.GraphQL.Draft.Syntax (ExecutableDefinition, Field, GType, Name, TypeDefinition)
import Language.GraphQL.Draft.Syntax qualified as G

-- | Analysis and FieldAnalysis are almost similar, except, Analysis will have a bool field (in future)
--   to indicate whether the GQL query is valid or not
data Analysis var = Analysis
  { -- | ordered hashmap from the fields to it's definition and analysis
    _aFields :: InsOrdHashMap FieldName (FieldDef, Maybe (FieldAnalysis var)),
    -- | map of all the variables, their type and default value (if there is any)
    _aVars :: HashMap VarName (GType, Maybe (G.Value Void)),
    _aErrs :: [Text]
  }
  deriving (Show)

data FieldAnalysis var = FieldAnalysis
  { _fFields :: InsOrdHashMap FieldName (FieldDef, Maybe (FieldAnalysis var)),
    _fVars :: HashMap VarName (GType, Maybe (G.Value var)),
    _fErrs :: [Text]
  }
  deriving (Show)

instance Monoid (FieldAnalysis v) where
  mempty = FieldAnalysis mempty mempty []

instance Semigroup (FieldAnalysis v) where
  (<>) fa1 fa2 = FieldAnalysis unionFields (_fVars fa1 <> _fVars fa2) (_fErrs fa1 <> _fErrs fa2)
    where
      unionFields = foldl' (safeInsertInFieldMap) (_fFields fa1) (OMap.toList (_fFields fa2))

instance Monoid (Analysis v) where
  mempty = Analysis mempty mempty []

instance Semigroup (Analysis v) where
  (<>) fa1 fa2 = Analysis unionFields (_aVars fa1 <> _aVars fa2) (_aErrs fa1 <> _aErrs fa2)
    where
      unionFields = foldl' (safeInsertInFieldMap) (_aFields fa1) (OMap.toList (_aFields fa2))

-- | FieldDef is analogous to `GType` from the `Language.GraphQL.Draft.Syntax` module
data FieldDef = FieldInfo G.Nullability (TypeDefinition [Name] RemoteSchemaInputValueDefinition) | FieldList G.Nullability FieldDef deriving (Show)

type FieldName = Name

type VarName = Name

analyzeGraphqlQuery :: ExecutableDefinition Name -> RemoteSchemaIntrospection -> Maybe (Analysis Name)
analyzeGraphqlQuery (G.ExecutableDefinitionOperation (G.OperationDefinitionTyped td)) sc = do
  let t = (G._todType td,) <$> G._todSelectionSet td
      varDefs = G._todVariableDefinitions td
      varMapList = map (\v -> (G._vdName v, (G._vdType v, G._vdDefaultValue v))) varDefs
      varMap = Map.fromList varMapList
      (fieldMap, errs) = getFieldsMap sc t
  pure Analysis {_aFields = fieldMap, _aVars = varMap, _aErrs = errs}
analyzeGraphqlQuery _ _ = Nothing

getAllAnalysisErrs :: Analysis Name -> [Text]
getAllAnalysisErrs Analysis {..} = _aErrs <> getFieldErrs (OMap.toList _aFields) []
  where
    getFieldErrs :: [(G.Name, (FieldDef, Maybe (FieldAnalysis G.Name)))] -> [Text] -> [Text]
    getFieldErrs [] lst = lst
    getFieldErrs ((_, (_, Just FieldAnalysis {..})) : xs) lst = _fErrs <> (getFieldErrs (OMap.toList _fFields) []) <> (getFieldErrs xs lst)
    getFieldErrs ((_, (_, Nothing)) : xs) lst = getFieldErrs xs lst

-- | inserts in field map, if there is already a key, then take a union of the fields inside
--   i.e. for the following graphql query:
--      query MyQuery {
--        test {
--          a
--          b
--        }
--        test {
--          b
--          c
--        }
--      }
--   The final field map of `test` will have a, b and c
safeInsertInFieldMap ::
  (Eq k, Hashable k, Semigroup a1) =>
  InsOrdHashMap k (a2, Maybe a1) ->
  (k, (a2, Maybe a1)) ->
  InsOrdHashMap k (a2, Maybe a1)
safeInsertInFieldMap m (k, v) =
  OMap.insertWith (\(fdef, (f1)) (_, (f2)) -> (fdef, f1 <> f2)) k v m

getFieldName :: Field frag var -> Name
getFieldName f = fromMaybe (G._fName f) (G._fAlias f)

getFieldsMap ::
  RemoteSchemaIntrospection ->
  [(G.OperationType, G.Selection frag var)] ->
  (InsOrdHashMap FieldName (FieldDef, Maybe (FieldAnalysis var)), [Text])
getFieldsMap rs ss =
  foldl'
    ( \(m, e) (o, f) -> case lookupRoot rs (o, f) of
        Left x0 -> (safeInsertInFieldMap m (getFieldName f, x0), e)
        Right txts -> (m, e <> txts)
    )
    (OMap.empty, [])
    fields
  where
    fields = mapMaybe (\(o, s) -> (o,) <$> field s) ss

getFieldsTypeM :: Name -> TypeDefinition possibleTypes inputType -> Maybe GType
getFieldsTypeM fieldName operationDefinitionSum = do
  operationDefinitionObject <- asObjectTypeDefinition operationDefinitionSum
  fieldDefinition <- find ((== fieldName) . G._fldName) $ G._otdFieldsDefinition operationDefinitionObject
  pure $ G._fldType fieldDefinition

lookupRoot ::
  RemoteSchemaIntrospection ->
  (G.OperationType, G.Field frag var) ->
  Either (FieldDef, Maybe (FieldAnalysis var)) [Text]
lookupRoot rs (ot, f) = do
  let rootFieldName = getRootFieldNameFromOpType ot
      fieldName = G._fName f
      fieldTypeM = do
        operationDefinitionSum <- lookupRS rs rootFieldName
        getFieldsTypeM fieldName operationDefinitionSum

  case fieldTypeM of
    Nothing ->
      case isMetaField fieldName True of
        Nothing -> Right $ ["Couldn't find field " <> G.unName fieldName <> " in root field " <> G.unName rootFieldName]
        Just fld -> lookupDefinition rs fld f
    Just fieldType -> lookupDefinition rs fieldType f

isMetaField :: Name -> Bool -> Maybe GType
isMetaField nam isRoot = do
  n <- fieldTypeMetaFields $ nam
  case G.unName nam of
    "__schema" -> if isRoot then Just $ mkGType n else Nothing
    "__type" -> if isRoot then Just $ mkGType n else Nothing
    "__typename" -> Just $ mkGType n
    _ -> Nothing
  where
    mkGType :: Name -> GType
    mkGType fName =
      G.TypeNamed
        (G.Nullability {unNullability = False})
        fName

fieldTypeMetaFields :: Name -> Maybe Name
fieldTypeMetaFields nam
  | (Just nam) == G.mkName "__schema" = G.mkName "__Schema"
  | (Just nam) == G.mkName "__type" = G.mkName "__Type"
  | (Just nam) == G.mkName "__typename" = G.mkName "String"
  | otherwise = Nothing

lookupDefinition ::
  RemoteSchemaIntrospection ->
  GType ->
  G.Field frag var ->
  Either (FieldDef, Maybe (FieldAnalysis var)) [Text]
lookupDefinition rs t f = do
  typeToSchemaM t \n ->
    case lookupRS rs n of
      Nothing -> Right ["Cannot find type definition for " <> G.unName n <> " in field " <> G.unName (G._fName f)]
      Just tDef -> Left $ getDefinition rs tDef (G._fSelectionSet f)

field :: G.Selection frag var -> Maybe (G.Field frag var)
field (G.SelectionField f) = Just f
field _ = Nothing

infixl 7 &&&&

(&&&&) :: Applicative f => (t -> f a1) -> (t -> f a2) -> t -> f (a1, a2)
f &&&& g = \a -> (,) <$> f a <*> g a

getRootFieldNameFromOpType :: G.OperationType -> G.Name
getRootFieldNameFromOpType G.OperationTypeQuery = $$(G.litName "query_root")
getRootFieldNameFromOpType G.OperationTypeMutation = $$(G.litName "mutation_root")
getRootFieldNameFromOpType G.OperationTypeSubscription = $$(G.litName "subscription_root")

asObjectTypeDefinition :: G.TypeDefinition possibleTypes inputType -> Maybe (G.ObjectTypeDefinition inputType)
asObjectTypeDefinition (G.TypeDefinitionObject o) = Just o
asObjectTypeDefinition _ = Nothing

lookupRS :: RemoteSchemaIntrospection -> G.Name -> Maybe (TypeDefinition [Name] RemoteSchemaInputValueDefinition)
lookupRS (RemoteSchemaIntrospection rsDefs) n = Map.lookup n rsDefs

typeToSchemaM ::
  GType ->
  (Name -> Either (TypeDefinition [Name] RemoteSchemaInputValueDefinition, Maybe (FieldAnalysis var)) [Text]) ->
  Either (FieldDef, Maybe (FieldAnalysis var)) [Text]
typeToSchemaM (G.TypeNamed n tName) k = case k tName of
  Right x -> Right x
  Left (t, x) -> Left (FieldInfo n t, x)
typeToSchemaM (G.TypeList n t) k = case typeToSchemaM t k of
  Right x -> Right x
  Left (t', x) -> Left (FieldList n t', x)

getDefinition ::
  RemoteSchemaIntrospection ->
  TypeDefinition [Name] RemoteSchemaInputValueDefinition ->
  G.SelectionSet frag var ->
  (TypeDefinition [Name] RemoteSchemaInputValueDefinition, Maybe (FieldAnalysis var))
getDefinition rs td sels =
  (td,) $ case td of
    (G.TypeDefinitionObject otd) -> do
      ps <-
        for
          sels
          \sel -> case (lookupFieldBySelection sel otd) of
            Left txts -> pure $ FieldAnalysis mempty mempty txts
            Right fd -> (mkFieldAnalysis rs sel fd)
      pure $ fold ps
    _ -> Nothing

itrListWith :: GType -> (Name -> p) -> p
itrListWith (G.TypeNamed _ tName) k = k tName
itrListWith (G.TypeList _ t) k = itrListWith t k

getFieldVars :: G.FieldDefinition RemoteSchemaInputValueDefinition -> HashMap Name (G.Value var) -> [(VarName, (GType, Maybe (G.Value var)))]
getFieldVars fDef' agMap =
  map
    ( \RemoteSchemaInputValueDefinition {..} ->
        (G._ivdName _rsitdDefinition, (G._ivdType _rsitdDefinition, Map.lookup (G._ivdName _rsitdDefinition) agMap))
    )
    (G._fldArgumentsDefinition fDef')

mkFieldAnalysis ::
  RemoteSchemaIntrospection ->
  G.Selection frag var ->
  G.FieldDefinition RemoteSchemaInputValueDefinition ->
  Maybe (FieldAnalysis var)
-- TODO: Handle `SelectionFragmentSpread` and `SelectionInlineFragment` as well
mkFieldAnalysis _ (G.SelectionFragmentSpread _) _ = Nothing
mkFieldAnalysis _ (G.SelectionInlineFragment _) _ = Nothing
mkFieldAnalysis rs (G.SelectionField f) fd = do
  let ag = G._fArguments f
      ft = G._fldType fd
      fn = getFieldName f
      (fFds, fVrs, fErrs) = itrListWith ft \n ->
        case lookupRS rs n of
          Nothing -> (mempty, mempty, ["Couldn't find definition for type " <> G.unName n <> " in field " <> G.unName fn <> " selected by " <> G.unName (G._fName f)])
          Just tDef ->
            let (def, fAn) = getDefinition rs tDef (G._fSelectionSet f)
             in case ft of
                  G.TypeNamed n' _ ->
                    (OMap.singleton fn (FieldInfo n' def, fAn), Map.fromList (getFieldVars fd ag), [])
                  G.TypeList n' _ ->
                    (OMap.singleton fn (FieldList n' $ FieldInfo n' def, fAn), Map.fromList (getFieldVars fd ag), [])

  pure
    FieldAnalysis
      { _fFields = fFds,
        _fVars = fVrs,
        _fErrs = fErrs
      }

lookupFieldBySelection :: G.Selection frag0 var0 -> G.ObjectTypeDefinition RemoteSchemaInputValueDefinition -> Either [Text] (G.FieldDefinition RemoteSchemaInputValueDefinition)
lookupFieldBySelection (G.SelectionField f) otd = case find (\d -> G._fName f == G._fldName d) lst of
  Nothing -> case isMetaField (G._fName f) False of
    Nothing -> Left ["Couldn't find definition for field " <> G.unName (getFieldName f) <> " in " <> G.unName (G._otdName otd)]
    Just gt -> Right $ mkFieldDef (G._fName f) gt
  Just fd -> Right fd
  where
    lst = G._otdFieldsDefinition otd
    mkFieldDef :: G.Name -> GType -> G.FieldDefinition RemoteSchemaInputValueDefinition
    mkFieldDef gName gt =
      G.FieldDefinition
        { _fldDescription = Nothing,
          _fldName = gName,
          _fldArgumentsDefinition = [],
          _fldType = gt,
          _fldDirectives = []
        }
lookupFieldBySelection _ _ = Left []
