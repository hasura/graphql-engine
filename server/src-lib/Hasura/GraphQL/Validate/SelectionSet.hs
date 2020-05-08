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
  -- , denormalizeSelectionSet
  , denormalizeObjectSelectionSet
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

  denormalizeField_
    :: ( MonadReader ValidationCtx m
       , MonadError QErr m
       , MonadReusability m
       , MonadState [G.Name] m
       )
    => a
    -> G.Field
    -> m (Maybe (DenormalizedField a))

  mergeSelections
    :: ( MonadReader ValidationCtx m
       , MonadError QErr m
       , MonadReusability m
       )
    => [Selection (DenormalizedField a) (DenormalizedSelectionSet a)]
    -> m (DenormalizedSelectionSet a)

  fromObjectSelectionSet
    :: G.NamedType
    -- ^ parent typename
    -> G.NamedType
    -- ^ fragment typename
    -> Set.HashSet G.NamedType
    -> DenormalizedSelectionSet ObjTyInfo
    -> DenormalizedSelectionSet a

  fromInterfaceSelectionSet
    :: G.NamedType
    -- ^ parent typename
    -> G.NamedType
    -- ^ fragment typename
    -> Set.HashSet G.NamedType
    -> DenormalizedSelectionSet IFaceTyInfo
    -> DenormalizedSelectionSet a

  fromUnionSelectionSet
    :: G.NamedType
    -- ^ parent typename
    -> G.NamedType
    -- ^ fragment typename
    -> Set.HashSet G.NamedType
    -> DenormalizedSelectionSet UnionTyInfo
    -> DenormalizedSelectionSet a

denormalizeObjectSelectionSet
  :: ( MonadError QErr m
     , MonadReusability m
     )
  => ValidationCtx
  -> ObjTyInfo
  -> G.SelectionSet
  -> m ObjectSelectionSet
denormalizeObjectSelectionSet validationCtx objectTypeInfo selectionSet =
  flip evalStateT [] $ flip runReaderT validationCtx $
    denormalizeSelectionSet objectTypeInfo selectionSet

denormalizeSelectionSet
  :: ( MonadReader ValidationCtx m
     , MonadError QErr m
     , MonadReusability m
     , HasSelectionSet a
     , MonadState [G.Name] m
     )
  => a
  -> G.SelectionSet
  -> m (DenormalizedSelectionSet a)
denormalizeSelectionSet fieldTypeInfo selectionSet =
  withPathK "selectionSet" $ do
    resolvedSelections <- catMaybes <$>
      mapM (denormalizeSelection fieldTypeInfo) selectionSet
    mergeSelections resolvedSelections

-- | While interfaces and objects have fields, unions do not, so
-- this is a specialized function for every Object type
denormalizeSelection
  :: ( MonadReader ValidationCtx m
     , MonadError QErr m
     , MonadReusability m
     , MonadState [G.Name] m
     , HasSelectionSet a
     )
  => a -- parent type info
  -> G.Selection
  -> m (Maybe (DenormalizedSelection a))
denormalizeSelection parentTypeInfo = \case
  G.SelectionField fld -> withPathK (G.unName $ G._fName fld) $ do
    let fieldName = G._fName fld
        fieldAlias = fromMaybe (G.Alias fieldName) $ G._fAlias fld
    fmap (SelectionField fieldAlias) <$> denormalizeField_ parentTypeInfo fld
  G.SelectionFragmentSpread (G.FragmentSpread name directives) -> do
    FragDef _ fragmentTyInfo fragmentSelectionSet <- getFragmentInfo name
    withPathK (G.unName name) $
      fmap (SelectionFragmentSpread name) <$>
      denormalizeFragment parentTypeInfo fragmentTyInfo directives fragmentSelectionSet
  G.SelectionInlineFragment (G.InlineFragment {..}) -> do
    let fragmentType = fromMaybe (getTypename parentTypeInfo) _ifTypeCondition
    fragmentTyInfo <- getFragmentTyInfo fragmentType
    withPathK "inlineFragment" $ fmap SelectionInlineFragmentSpread <$>
      denormalizeFragment parentTypeInfo fragmentTyInfo _ifDirectives _ifSelectionSet

denormalizeFragment
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
  -> m (Maybe (DenormalizedSelectionSet a))
denormalizeFragment parentTyInfo fragmentTyInfo directives fragmentSelectionSet = do
  commonTypes <- validateSpread
  case fragmentTyInfo of
    FragmentTyObject objTyInfo ->
      withDirectives directives $
      fmap (fromObjectSelectionSet parentType fragmentType commonTypes) $
      denormalizeSelectionSet objTyInfo fragmentSelectionSet
    FragmentTyInterface interfaceTyInfo ->
      withDirectives directives $
      fmap (fromInterfaceSelectionSet parentType fragmentType commonTypes) $
      denormalizeSelectionSet interfaceTyInfo fragmentSelectionSet
    FragmentTyUnion unionTyInfo ->
      withDirectives directives $
      fmap (fromUnionSelectionSet parentType fragmentType commonTypes) $
      denormalizeSelectionSet unionTyInfo fragmentSelectionSet
  where
    validateSpread = do
      let commonTypes = parentTypeMembers `Set.intersection` fragmentTypeMembers
      if null commonTypes then
          -- TODO: fragment source
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
    -- groups :: OMap.InsOrdHashMap G.Alias (NE.NESeq f)
    groups = foldr (OMap.unionWith (<>)) mempty $
             map (fmap NE.init . unAliasedFields) flds

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
  case NE.head selectionSets of
    SelectionSetObject s ->
      SelectionSetObject <$>
      assertingSimilar s getObjectSelectionSet mergeObjectSelectionSets
    SelectionSetUnion s ->
      SelectionSetUnion <$>
      assertingSimilar s getUnionSelectionSet mergeUnionSelectionSets
    SelectionSetInterface s ->
      SelectionSetInterface <$>
      assertingSimilar s getInterfaceSelectionSet mergeInterfaceSelectionSets
    SelectionSetNone ->
      if all (== SelectionSetNone) $ NE.tail selectionSets
         then pure SelectionSetNone
         else throw500 $ "mergeSelectionSets: 'same kind' assertion failed"
  where
    assertingSimilar
      :: MonadError QErr m
      => s -> (SelectionSet -> Maybe s) -> ([s] -> m s) -> m s
    assertingSimilar s l f = do
      let sameSelectionSets = mapMaybe l $ toList $ NE.tail selectionSets
      if length sameSelectionSets == length (NE.tail selectionSets)
         then f (s:sameSelectionSets)
         else throw500 $ "mergeSelectionSets: 'same kind' assertion failed"

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

mergeInterfaceSelectionSets
  :: (MonadError QErr m) => [InterfaceSelectionSet] -> m InterfaceSelectionSet
mergeInterfaceSelectionSets = mergeScopedSelectionSets

mergeUnionSelectionSets
  :: (MonadError QErr m) => [UnionSelectionSet] -> m UnionSelectionSet
mergeUnionSelectionSets = mergeScopedSelectionSets

withDirectives
  :: ( MonadReader ValidationCtx m
     , MonadError QErr m
     , MonadReusability m
     )
  => [G.Directive]
  -> m a
  -> m (Maybe a)
withDirectives dirs act = withPathK "directives" $ do
  dirDefs <- onLeft (mkMapWith G._dName dirs) $ \dups ->
    throwVE $ "the following directives are used more than once: " <>
    showNames dups

  procDirs <- flip Map.traverseWithKey dirDefs $ \name dir ->
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
      SelectionSetObject <$> denormalizeSelectionSet objTyInfo selSet

    (TIIFace _, [])  ->
      throwVE $ "field " <> showName name <> " of type "
      <> G.showGT fldTy <> " must have a selection of subfields"

    (TIIFace interfaceTyInfo, _) ->
      SelectionSetInterface <$> denormalizeSelectionSet interfaceTyInfo selSet

    (TIUnion _, [])  ->
      throwVE $ "field " <> showName name <> " of type "
      <> G.showGT fldTy <> " must have a selection of subfields"

    (TIUnion unionTyInfo, _) ->
      SelectionSetUnion <$> denormalizeSelectionSet unionTyInfo selSet

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

-- inlineFragmentInObjectScope
--   :: ( MonadReader ValidationCtx m
--      , MonadError QErr m
--      , MonadReusability m
--      , MonadState [G.Name] m
--      )
--   => ObjTyInfo -- type information of the field
--   -> G.InlineFragment
--   -> m (Maybe ObjectSelectionSet)
-- inlineFragmentInObjectScope fldTyInfo inlnFrag = do
--   let fldTy  = _otiName fldTyInfo
--   let fragmentType = fromMaybe fldTy tyM
--   when (fldTy /= fragmentType) $
--     throwVE $ "inline fragment is expected on type " <>
--     showNamedTy fldTy <> " but found " <> showNamedTy fragmentType
--   withDirectives directives $ denormalizeObjectSelectionSet fldTyInfo selSet
--   where
--     G.InlineFragment tyM directives selSet = inlnFrag

type instance DenormalizedSelectionSet ObjTyInfo = ObjectSelectionSet
type instance DenormalizedField ObjTyInfo = Field
instance HasSelectionSet ObjTyInfo where

  getTypename = _otiName
  getMemberTypes = Set.singleton . _otiName

  denormalizeField_ objTyInfo field = do
    fieldInfo <- getFieldInfo (_otiName objTyInfo) (_otiFields objTyInfo) $ G._fName field
    denormalizeField fieldInfo field

  mergeSelections selections =
    mergeObjectSelectionSets $ map toObjectSelectionSet selections
    where
      toObjectSelectionSet = \case
        SelectionField alias fld ->
          ObjectSelectionSet $ AliasedFields $ OMap.singleton alias fld
        SelectionInlineFragmentSpread selectionSet -> selectionSet
        SelectionFragmentSpread _ selectionSet -> selectionSet

  fromObjectSelectionSet _ _ _ objectSelectionSet =
    objectSelectionSet

  fromInterfaceSelectionSet parentType _ _ interfaceSelectionSet =
      getMemberSelectionSet parentType interfaceSelectionSet

  fromUnionSelectionSet parentType _ _ unionSelectionSet =
      getMemberSelectionSet parentType unionSelectionSet

type instance DenormalizedSelectionSet IFaceTyInfo = InterfaceSelectionSet
type instance DenormalizedField IFaceTyInfo = Field

instance HasSelectionSet IFaceTyInfo where

  getTypename = _ifName
  -- TODO
  getMemberTypes = _ifMemberTypes

  denormalizeField_ interfaceTyInfo field = do
    fieldInfo <- getFieldInfo (_ifName interfaceTyInfo) (_ifFields interfaceTyInfo)
                 $ G._fName field
    denormalizeField fieldInfo field

  mergeSelections selections =
    mergeInterfaceSelectionSets $ map toInterfaceSelectionSet selections
    where
      toInterfaceSelectionSet = \case
        SelectionField alias fld ->
          ScopedSelectionSet (AliasedFields $ OMap.singleton alias fld) mempty
        SelectionInlineFragmentSpread selectionSet -> selectionSet
        SelectionFragmentSpread _ selectionSet -> selectionSet

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

type instance DenormalizedSelectionSet UnionTyInfo = UnionSelectionSet
type instance DenormalizedField UnionTyInfo = Typename

instance HasSelectionSet UnionTyInfo where

  getTypename = _utiName
  getMemberTypes = _utiMemberTypes

  denormalizeField_ unionTyInfo field = do
    let fieldMap = Map.singleton (_fiName typenameFld) typenameFld
    fieldInfo <- getFieldInfo (_utiName unionTyInfo) fieldMap $ G._fName field
    fmap (const Typename) <$> denormalizeField fieldInfo field

  mergeSelections selections =
    mergeUnionSelectionSets $ map toUnionSelectionSet selections
    where
      toUnionSelectionSet = \case
        SelectionField alias fld ->
          ScopedSelectionSet (AliasedFields $ OMap.singleton alias fld) mempty
        SelectionInlineFragmentSpread selectionSet -> selectionSet
        SelectionFragmentSpread _ selectionSet -> selectionSet

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

