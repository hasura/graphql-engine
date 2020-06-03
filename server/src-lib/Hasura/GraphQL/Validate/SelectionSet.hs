{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RecordWildCards #-}
module Hasura.GraphQL.Validate.SelectionSet
  ( ArgsMap
  , Field(..)
  , AliasedFields(..)
  , SelectionSet(..)
  , ObjectSelectionSet(..)
  , traverseObjectSelectionSet
  , InterfaceSelectionSet
  , UnionSelectionSet
  , RootSelectionSet(..)
  , parseObjectSelectionSet
  , asObjectSelectionSet
  ) where

import           Hasura.Prelude

import qualified Data.Text                           as T
import qualified Data.HashMap.Strict                 as Map
import qualified Data.HashSet                        as Set
import qualified Data.HashMap.Strict.InsOrd.Extended as OMap
import qualified Language.GraphQL.Draft.Syntax       as G
import qualified Data.Sequence.NonEmpty              as NE
import qualified Data.List                           as L

import           Hasura.GraphQL.Validate.Context
import           Hasura.GraphQL.Validate.Types
import           Hasura.GraphQL.Validate.InputValue
import           Hasura.GraphQL.NormalForm
import           Hasura.RQL.Types
import           Hasura.SQL.Value

class HasSelectionSet a where

  getTypename :: a -> G.NamedType
  getMemberTypes :: a -> Set.HashSet G.NamedType

  fieldToSelectionSet
    :: G.Alias -> NormalizedField a -> NormalizedSelectionSet a

  parseField_
    :: ( MonadReader ValidationCtx m
       , MonadError QErr m
       , MonadReusability m
       , MonadState [G.Name] m
       )
    => a
    -> G.Field
    -> m (Maybe (NormalizedField a))

  mergeNormalizedSelectionSets
    :: ( MonadReader ValidationCtx m
       , MonadError QErr m
       , MonadReusability m
       )
    => [NormalizedSelectionSet a]
    -> m (NormalizedSelectionSet a)

  fromObjectSelectionSet
    :: G.NamedType
    -- ^ parent typename
    -> G.NamedType
    -- ^ fragment typename
    -> Set.HashSet G.NamedType
    -- ^ common types
    -> NormalizedSelectionSet ObjTyInfo
    -> NormalizedSelectionSet a

  fromInterfaceSelectionSet
    :: G.NamedType
    -- ^ parent typename
    -> G.NamedType
    -- ^ fragment typename
    -> Set.HashSet G.NamedType
    -> NormalizedSelectionSet IFaceTyInfo
    -> NormalizedSelectionSet a

  fromUnionSelectionSet
    :: G.NamedType
    -- ^ parent typename
    -> G.NamedType
    -- ^ fragment typename
    -> Set.HashSet G.NamedType
    -- ^ common types
    -> NormalizedSelectionSet UnionTyInfo
    -> NormalizedSelectionSet a

parseObjectSelectionSet
  :: ( MonadError QErr m
     , MonadReusability m
     )
  => ValidationCtx
  -> ObjTyInfo
  -> G.SelectionSet
  -> m ObjectSelectionSet
parseObjectSelectionSet validationCtx objectTypeInfo selectionSet =
  flip evalStateT [] $ flip runReaderT validationCtx $
    parseSelectionSet objectTypeInfo selectionSet

selectionToSelectionSet
  :: HasSelectionSet a
  => NormalizedSelection a -> NormalizedSelectionSet a
selectionToSelectionSet = \case
  SelectionField alias fld -> fieldToSelectionSet alias fld
  SelectionInlineFragmentSpread selectionSet -> selectionSet
  SelectionFragmentSpread _ selectionSet -> selectionSet

parseSelectionSet
  :: ( MonadReader ValidationCtx m
     , MonadError QErr m
     , MonadReusability m
     , HasSelectionSet a
     , MonadState [G.Name] m
     )
  => a
  -> G.SelectionSet
  -> m (NormalizedSelectionSet a)
parseSelectionSet fieldTypeInfo selectionSet =
  withPathK "selectionSet" $ do
    normalizedSelections <- catMaybes <$> mapM (parseSelection fieldTypeInfo) selectionSet
    mergeNormalizedSelections normalizedSelections
  where
    mergeNormalizedSelections = mergeNormalizedSelectionSets . map selectionToSelectionSet

-- | While interfaces and objects have fields, unions do not, so
-- this is a specialized function for every Object type
parseSelection
  :: ( MonadReader ValidationCtx m
     , MonadError QErr m
     , MonadReusability m
     , MonadState [G.Name] m
     , HasSelectionSet a
     )
  => a -- parent type info
  -> G.Selection
  -> m (Maybe (NormalizedSelection a))
parseSelection parentTypeInfo = \case
  G.SelectionField fld -> withPathK (G.unName $ G._fName fld) $ do
    let fieldName = G._fName fld
        fieldAlias = fromMaybe (G.Alias fieldName) $ G._fAlias fld
    fmap (SelectionField fieldAlias) <$> parseField_ parentTypeInfo fld
  G.SelectionFragmentSpread (G.FragmentSpread name directives) -> do
    FragDef _ fragmentTyInfo fragmentSelectionSet <- getFragmentInfo name
    withPathK (G.unName name) $
      fmap (SelectionFragmentSpread name) <$>
      parseFragment parentTypeInfo fragmentTyInfo directives fragmentSelectionSet
  G.SelectionInlineFragment (G.InlineFragment {..}) -> do
    let fragmentType = fromMaybe (getTypename parentTypeInfo) _ifTypeCondition
    fragmentTyInfo <- getFragmentTyInfo fragmentType
    withPathK "inlineFragment" $ fmap SelectionInlineFragmentSpread <$>
      parseFragment parentTypeInfo fragmentTyInfo _ifDirectives _ifSelectionSet

parseFragment
  :: ( MonadReader ValidationCtx m
     , MonadError QErr m
     , MonadReusability m
     , MonadState [G.Name] m
     , HasSelectionSet a
     )
  => a
  -> FragmentTypeInfo
  -> [G.Directive]
  -> G.SelectionSet
  -> m (Maybe (NormalizedSelectionSet a))
parseFragment parentTyInfo fragmentTyInfo directives fragmentSelectionSet = do
  commonTypes <- validateSpread
  case fragmentTyInfo of
    FragmentTyObject objTyInfo ->
      withDirectives directives $
      fmap (fromObjectSelectionSet parentType fragmentType commonTypes) $
      parseSelectionSet objTyInfo fragmentSelectionSet
    FragmentTyInterface interfaceTyInfo ->
      withDirectives directives $
      fmap (fromInterfaceSelectionSet parentType fragmentType commonTypes) $
      parseSelectionSet interfaceTyInfo fragmentSelectionSet
    FragmentTyUnion unionTyInfo ->
      withDirectives directives $
      fmap (fromUnionSelectionSet parentType fragmentType commonTypes) $
      parseSelectionSet unionTyInfo fragmentSelectionSet
  where
    validateSpread = do
      let commonTypes = parentTypeMembers `Set.intersection` fragmentTypeMembers
      if null commonTypes then
          -- TODO: better error location by capturing the fragment source -
          -- named or otherwise
          -- throwVE $ "cannot spread fragment " <> showName name <> " defined on " <>
          throwVE $ "cannot spread fragment  defined on " <> showNamedTy fragmentType
          <> " when selecting fields of type " <> showNamedTy parentType
         else pure commonTypes

    parentType = getTypename parentTyInfo
    parentTypeMembers = getMemberTypes parentTyInfo

    fragmentType = case fragmentTyInfo of
      FragmentTyObject tyInfo -> getTypename tyInfo
      FragmentTyInterface tyInfo -> getTypename tyInfo
      FragmentTyUnion tyInfo -> getTypename tyInfo
    fragmentTypeMembers = case fragmentTyInfo of
      FragmentTyObject tyInfo -> getMemberTypes tyInfo
      FragmentTyInterface tyInfo -> getMemberTypes tyInfo
      FragmentTyUnion tyInfo -> getMemberTypes tyInfo

class IsField f => MergeableField f where

  checkFieldMergeability
    :: (MonadError QErr m) => G.Alias -> NE.NESeq f -> m f

instance MergeableField Field where

  checkFieldMergeability alias fields = do
    let groupedFlds = toList $ NE.toSeq fields
        fldNames = L.nub $ map getFieldName groupedFlds
        args = L.nub $ map getFieldArguments groupedFlds
    when (length fldNames > 1) $
      throwVE $ "cannot merge different fields under the same alias ("
      <> showName (G.unAlias alias) <> "): "
      <> showNames fldNames
    when (length args > 1) $
      throwVE $ "cannot merge fields with different arguments"
      <> " under the same alias: "
      <> showName (G.unAlias alias)
    let fld = NE.head fields
    mergedGroupSelectionSet <- mergeSelectionSets $ fmap _fSelSet fields
    return $ fld { _fSelSet = mergedGroupSelectionSet }

instance MergeableField Typename where

  checkFieldMergeability _ fields = pure $ NE.head fields

parseArguments
  :: ( MonadReader ValidationCtx m
     , MonadError QErr m
     )
  => ParamMap
  -> [G.Argument]
  -> m ArgsMap
parseArguments fldParams argsL = do

  args <- onLeft (mkMapWith G._aName argsL) $ \dups ->
    throwVE $ "the following arguments are defined more than once: " <>
    showNames dups

  let requiredParams = Map.filter (G.isNotNull . _iviType) fldParams

  inpArgs <- forM args $ \(G.Argument argName argVal) ->
    withPathK (G.unName argName) $ do
      argTy <- getArgTy argName
      validateInputValue valueParser argTy argVal

  forM_ requiredParams $ \argDef -> do
    let param = _iviName argDef
    onNothing (Map.lookup param inpArgs) $ throwVE $ mconcat
      [ "the required argument ", showName param, " is missing"]

  return inpArgs

  where
    getArgTy argName =
      onNothing (_iviType <$> Map.lookup argName fldParams) $ throwVE $
      "no such argument " <> showName argName <> " is expected"

mergeFields
  :: ( MonadError QErr m
     , MergeableField f
     )
  -- => Seq.Seq Field
  => [AliasedFields f]
  -> m (AliasedFields f)
mergeFields flds =
  AliasedFields <$> OMap.traverseWithKey checkFieldMergeability groups
  where
    groups = foldr (OMap.unionWith (<>)) mempty $
             map (fmap NE.init . unAliasedFields) flds

appendSelectionSets
  :: (MonadError QErr m) => SelectionSet -> SelectionSet -> m SelectionSet
appendSelectionSets = curry \case
  (SelectionSetObject s1, SelectionSetObject s2) ->
    SelectionSetObject <$> mergeObjectSelectionSets [s1, s2]
  (SelectionSetInterface s1, SelectionSetInterface s2) ->
    SelectionSetInterface <$> appendScopedSelectionSet s1 s2
  (SelectionSetUnion s1, SelectionSetUnion s2) ->
    SelectionSetUnion <$> appendScopedSelectionSet s1 s2
  (SelectionSetNone, SelectionSetNone) -> pure SelectionSetNone
  (_, _) -> throw500 $ "mergeSelectionSets: 'same kind' assertion failed"


-- query q {
--    author {
--      id
--    }
--    author {
--      name
--    }
-- }
--
-- | When we are merging two selection sets down two different trees they
-- should be of the same type, however, as it is not enforced in the type
-- system, an internal error is thrown when this assumption is violated
mergeSelectionSets
  :: (MonadError QErr m) => NE.NESeq SelectionSet -> m SelectionSet
-- mergeSelectionSets = curry $ \case
mergeSelectionSets selectionSets =
  foldM appendSelectionSets (NE.head selectionSets) $ NE.tail selectionSets

mergeObjectSelectionSets
  :: (MonadError QErr m) => [ObjectSelectionSet] -> m ObjectSelectionSet
mergeObjectSelectionSets =
  fmap ObjectSelectionSet . mergeFields . map unObjectSelectionSet

mergeObjectSelectionSetMaps
  :: (MonadError QErr m) => [ObjectSelectionSetMap] -> m ObjectSelectionSetMap
mergeObjectSelectionSetMaps selectionSetMaps =
  traverse mergeObjectSelectionSets $
    foldr (Map.unionWith (<>)) mempty $ map (fmap (:[])) selectionSetMaps

appendScopedSelectionSet
  :: (MonadError QErr m, MergeableField f)
  => ScopedSelectionSet f -> ScopedSelectionSet f -> m (ScopedSelectionSet f)
appendScopedSelectionSet s1 s2 =
  ScopedSelectionSet
  <$> mergeFields [_sssBaseSelectionSet s1, _sssBaseSelectionSet s2]
  <*> mergeObjectSelectionSetMaps [s1MembersUnified, s2MembersUnified]

  where
    s1Base = fmap toField $ _sssBaseSelectionSet s1
    s2Base = fmap toField $ _sssBaseSelectionSet s2

    s1MembersUnified =
      (_sssMemberSelectionSets s1)
      <> fmap (const (ObjectSelectionSet s1Base)) (_sssMemberSelectionSets s2)

    s2MembersUnified =
      (_sssMemberSelectionSets s2)
      <> fmap (const (ObjectSelectionSet s2Base)) (_sssMemberSelectionSets s1)

mergeScopedSelectionSets
  :: (MonadError QErr m, MergeableField f)
  => [ScopedSelectionSet f] -> m (ScopedSelectionSet f)
mergeScopedSelectionSets selectionSets =
  foldM appendScopedSelectionSet emptyScopedSelectionSet selectionSets

withDirectives
  :: ( MonadReader ValidationCtx m
     , MonadError QErr m
     , MonadReusability m
     )
  => [G.Directive]
  -> m a
  -> m (Maybe a)
withDirectives dirs act = do
  procDirs <- withPathK "directives" $ do
    dirDefs <- onLeft (mkMapWith G._dName dirs) $ \dups ->
      throwVE $ "the following directives are used more than once: " <>
      showNames dups

    flip Map.traverseWithKey dirDefs $ \name dir ->
      withPathK (G.unName name) $ do
        dirInfo <- onNothing (Map.lookup (G._dName dir) defDirectivesMap) $
                   throwVE $ "unexpected directive: " <> showName name
        procArgs <- withPathK "args" $ parseArguments (_diParams dirInfo)
                    (G._dArguments dir)
        getIfArg procArgs

  let shouldSkip    = fromMaybe False $ Map.lookup "skip" procDirs
      shouldInclude = fromMaybe True $ Map.lookup "include" procDirs

  if not shouldSkip && shouldInclude
    then Just <$> act
    else return Nothing

  where
    getIfArg m = do
      val <- onNothing (Map.lookup "if" m) $ throw500
              "missing if argument in the directive"
      when (isJust $ _aivVariable val) markNotReusable
      case _aivValue val of
        AGScalar _ (Just (PGValBoolean v)) -> return v
        _ -> throw500 "did not find boolean scalar for if argument"

getFragmentInfo
  :: (MonadReader ValidationCtx m, MonadError QErr m, MonadState [G.Name] m)
  => G.Name
  -- ^ fragment name
  -> m FragDef
getFragmentInfo name = do
  -- check for cycles
  visitedFragments <- get
  if name `elem` visitedFragments
    then throwVE $ "cannot spread fragment " <> showName name
         <> " within itself via "
         <> T.intercalate "," (map G.unName visitedFragments)
    else put $ name:visitedFragments
  fragInfo <- Map.lookup name <$> asks _vcFragDefMap
  onNothing fragInfo $ throwVE $ "fragment '" <> G.unName name <> "' not found"

denormalizeField
  :: ( MonadReader ValidationCtx m
     , MonadError QErr m
     , MonadReusability m
     , MonadState [G.Name] m
     )
  => ObjFldInfo
  -> G.Field
  -> m (Maybe Field)
denormalizeField fldInfo (G.Field _ name args dirs selSet) = do

  let fldTy = _fiTy fldInfo
      fldBaseTy = getBaseTy fldTy

  fldTyInfo <- getTyInfo fldBaseTy

  argMap <- withPathK "args" $ parseArguments (_fiParams fldInfo) args

  fields <- case (fldTyInfo, selSet) of

    (TIObj _, [])  ->
      throwVE $ "field " <> showName name <> " of type "
      <> G.showGT fldTy <> " must have a selection of subfields"

    (TIObj objTyInfo, _) ->
      SelectionSetObject <$> parseSelectionSet objTyInfo selSet

    (TIIFace _, [])  ->
      throwVE $ "field " <> showName name <> " of type "
      <> G.showGT fldTy <> " must have a selection of subfields"

    (TIIFace interfaceTyInfo, _) ->
      SelectionSetInterface <$> parseSelectionSet interfaceTyInfo selSet

    (TIUnion _, [])  ->
      throwVE $ "field " <> showName name <> " of type "
      <> G.showGT fldTy <> " must have a selection of subfields"

    (TIUnion unionTyInfo, _) ->
      SelectionSetUnion <$> parseSelectionSet unionTyInfo selSet

    (TIScalar _, []) -> return SelectionSetNone
    -- when scalar/enum and no empty set
    (TIScalar _, _) ->
      throwVE $ "field " <> showName name <> " must not have a "
      <> "selection since type " <> G.showGT fldTy <> " has no subfields"

    (TIEnum _, []) -> return SelectionSetNone
    (TIEnum _, _) ->
      throwVE $ "field " <> showName name <> " must not have a "
      <> "selection since type " <> G.showGT fldTy <> " has no subfields"

    (TIInpObj _, _) ->
      throwVE $ "internal error: unexpected input type for field: "
      <> showName name

  withDirectives dirs $ pure $ Field  name fldBaseTy argMap fields

type instance NormalizedSelectionSet ObjTyInfo = ObjectSelectionSet
type instance NormalizedField ObjTyInfo = Field

instance HasSelectionSet ObjTyInfo where

  getTypename = _otiName
  getMemberTypes = Set.singleton . _otiName

  parseField_ objTyInfo field = do
    fieldInfo <- getFieldInfo (_otiName objTyInfo) (_otiFields objTyInfo) $ G._fName field
    denormalizeField fieldInfo field

  fieldToSelectionSet alias fld =
    ObjectSelectionSet $ AliasedFields $ OMap.singleton alias fld

  mergeNormalizedSelectionSets = mergeObjectSelectionSets

  fromObjectSelectionSet _ _ _ objectSelectionSet =
    objectSelectionSet

  fromInterfaceSelectionSet parentType _ _ interfaceSelectionSet =
      getMemberSelectionSet parentType interfaceSelectionSet

  fromUnionSelectionSet parentType _ _ unionSelectionSet =
      getMemberSelectionSet parentType unionSelectionSet

type instance NormalizedSelectionSet IFaceTyInfo = InterfaceSelectionSet
type instance NormalizedField IFaceTyInfo = Field

instance HasSelectionSet IFaceTyInfo where

  getTypename = _ifName
  getMemberTypes = _ifMemberTypes

  parseField_ interfaceTyInfo field = do
    fieldInfo <- getFieldInfo (_ifName interfaceTyInfo) (_ifFields interfaceTyInfo)
                 $ G._fName field
    denormalizeField fieldInfo field

  fieldToSelectionSet alias field =
    ScopedSelectionSet (AliasedFields $ OMap.singleton alias field) mempty

  mergeNormalizedSelectionSets = mergeScopedSelectionSets

  fromObjectSelectionSet _ fragmentType _ objectSelectionSet =
    ScopedSelectionSet (AliasedFields mempty) $
      Map.singleton fragmentType objectSelectionSet

  fromInterfaceSelectionSet _ _ commonTypes interfaceSelectionSet =
    ScopedSelectionSet (AliasedFields mempty) $
      Map.fromList $ flip map (toList commonTypes) $
      \commonType -> (commonType, getMemberSelectionSet commonType interfaceSelectionSet)

  fromUnionSelectionSet _ _ commonTypes unionSelectionSet =
    ScopedSelectionSet (AliasedFields mempty) $
      Map.fromList $ flip map (toList commonTypes) $
      \commonType -> (commonType, getMemberSelectionSet commonType unionSelectionSet)

type instance NormalizedSelectionSet UnionTyInfo = UnionSelectionSet
type instance NormalizedField UnionTyInfo = Typename

instance HasSelectionSet UnionTyInfo where

  getTypename = _utiName
  getMemberTypes = _utiMemberTypes

  parseField_ unionTyInfo field = do
    let fieldMap = Map.singleton (_fiName typenameFld) typenameFld
    fieldInfo <- getFieldInfo (_utiName unionTyInfo) fieldMap $ G._fName field
    fmap (const Typename) <$> denormalizeField fieldInfo field

  fieldToSelectionSet alias field =
    ScopedSelectionSet (AliasedFields $ OMap.singleton alias field) mempty

  mergeNormalizedSelectionSets = mergeScopedSelectionSets

  fromObjectSelectionSet _ fragmentType _ objectSelectionSet =
    ScopedSelectionSet (AliasedFields mempty) $
      Map.singleton fragmentType objectSelectionSet

  fromInterfaceSelectionSet _ _ commonTypes interfaceSelectionSet =
    ScopedSelectionSet (AliasedFields mempty) $
      Map.fromList $ flip map (toList commonTypes) $
      \commonType -> (commonType, getMemberSelectionSet commonType interfaceSelectionSet)

  fromUnionSelectionSet _ _ commonTypes unionSelectionSet =
    ScopedSelectionSet (AliasedFields mempty) $
      Map.fromList $ flip map (toList commonTypes) $
      \commonType -> (commonType, getMemberSelectionSet commonType unionSelectionSet)

