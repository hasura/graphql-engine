module Hasura.GraphQL.Validate.Field
  ( ArgsMap
  , Field(..)
  , SelSet
  , Located(..)
  , denormSelSet
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                          as J
import qualified Data.Aeson.Casing                   as J
import qualified Data.Aeson.TH                       as J
import qualified Data.HashMap.Strict                 as Map
import qualified Data.HashMap.Strict.InsOrd.Extended as OMap
import qualified Data.List                           as L
import qualified Data.Sequence                       as Seq
import qualified Data.Sequence.NonEmpty              as NE
import qualified Data.Text                           as T
import qualified Language.GraphQL.Draft.Syntax       as G

import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Validate.Context
import           Hasura.GraphQL.Validate.InputValue
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Value

-- data ScalarInfo
--   = SIBuiltin !GBuiltin
--   | SICustom !PGColType
--   deriving (Show, Eq)

-- data GBuiltin
--   = GInt
--   | GFloat
--   | GBoolean
--   | GString
--   deriving (Show, Eq)

data TypedOperation
  = TypedOperation
  { _toType         :: !G.OperationType
  , _toName         :: !(Maybe G.Name)
  , _toSelectionSet :: ![Field]
  } deriving (Show, Eq)

type ArgsMap = Map.HashMap G.Name AnnInpVal

type SelSet = Seq.Seq Field

-- N.B. This is a tree via 'SelSet'
-- | https://graphql.github.io/graphql-spec/June2018/#sec-Language.Fields 
data Field
  = Field
  { _fAlias     :: !G.Alias
  , _fName      :: !G.Name
  , _fType      :: !G.NamedType
  , _fArguments :: !ArgsMap
  , _fSelSet    :: !SelSet
  , _fRemoteRel :: !(Maybe RemoteField)
  } deriving (Eq, Show)

$(J.deriveToJSON (J.aesonDrop 2 J.camelCase){J.omitNothingFields=True}
  ''Field
 )

-- newtype FieldMapAlias
--   = FieldMapAlias
--   { unFieldMapAlias :: Map.HashMap G.Alias (FieldG FieldMapAlias)
--   } deriving (Show, Eq)

-- newtype FieldMapName
--   = FieldMapName
--   { unFieldMapName :: Map.HashMap G.Name (NE.NonEmpty (FieldG FieldMapName))
--   } deriving (Show, Eq)

-- type Field = FieldG FieldMapAlias

-- type FieldGrouped = FieldG FieldMapName

-- toFieldGrouped :: Field -> FieldGrouped
-- toFieldGrouped =
--   fmap groupFields
--   where
--     groupFields m =
--       FieldMapName $ groupTuples $
--       flip map (Map.elems $ unFieldMapAlias m) $ \fld ->
--       (_fName fld, toFieldGrouped fld)

data FieldGroupSrc
  = FGSFragSprd !G.Name
  | FGSInlnFrag
  deriving (Show, Eq)

data FieldGroup
  = FieldGroup
  { _fgSource :: !FieldGroupSrc
  , _fgFields :: !(Seq.Seq (Located Field))
  } deriving (Show, Eq)

-- data GLoc
--   = GLoc
--   { _glLine   :: !Int
--   , _glColumn :: !Int
--   } deriving (Show, Eq)

-- data GErr
--   = GErr
--   { _geMessage   :: !Text
--   , _geLocations :: ![GLoc]
--   } deriving (Show, Eq)

-- throwGE :: (MonadError QErr m) => Text -> m a
-- throwGE msg = throwError $ QErr msg []

withDirectives
  :: ( MonadReader ValidationCtx m
     , MonadError QErr m)
  => [G.Directive]
  -> m a
  -> m (Maybe a)
withDirectives dirs act = do

  dirDefs <- onLeft (mkMapWith G._dName dirs) $ \dups ->
    throwVE $ "the following directives are used more than once: " <>
    showNames dups

  procDirs <- flip Map.traverseWithKey dirDefs $ \name dir ->
    withPathK (G.unName name) $ do
      dirInfo <- onNothing (Map.lookup (G._dName dir) defDirectivesMap) $
                 throwVE $ "unexpected directive: " <> showName name
      procArgs <- withPathK "args" $ processArgs (_diParams dirInfo)
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
      case _aivValue val of
        AGScalar _ (Just (PGValBoolean v)) -> return v
        _ -> throw500 "did not find boolean scalar for if argument"

denormSel
  :: ( MonadReader ValidationCtx m
     , MonadError QErr m)
  => [G.Name] -- visited fragments
  -> ObjTyInfo -- parent type info
  -> G.Selection
  -> m (Maybe (Either (Located Field) FieldGroup))
denormSel visFrags parObjTyInfo sel = case sel of
  G.SelectionField fld -> withPathK (G.unName $ G._fName fld) $ do
    fldInfo <- getFieldInfo parObjTyInfo $ G._fName fld
    fmap Left . fmap (localize fldInfo) <$> denormFld parObjTyInfo visFrags fldInfo fld
  G.SelectionFragmentSpread fragSprd ->
    withPathK (G.unName $ G._fsName fragSprd) $
    fmap Right <$> denormFrag visFrags parTy fragSprd
  G.SelectionInlineFragment inlnFrag ->
    withPathK "inlineFragment" $
    fmap Right <$> denormInlnFrag visFrags parObjTyInfo inlnFrag
  where
    parTy = _otiName parObjTyInfo
    localize fldInfo field =
      case _fiLoc fldInfo of
        TLHasuraType                    -> HasuraLocated field
        TLRemoteType _ remoteSchemaInfo -> RemoteLocated remoteSchemaInfo field

processArgs
  :: ( MonadReader ValidationCtx m
     , MonadError QErr m)
  => ParamMap
  -> [G.Argument]
  -> m ArgsMap
processArgs fldParams argsL = do

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

denormFld
  :: ( MonadReader ValidationCtx m
     , MonadError QErr m)
  => ObjTyInfo
  -> [G.Name] -- visited fragments
  -> ObjFldInfo
  -> G.Field
  -> m (Maybe Field)
denormFld parObjTyInfo visFrags fldInfo (G.Field aliasM name args dirs selSet) = do

  let fldTy = _fiTy fldInfo
      fldBaseTy = getBaseTy fldTy

  fldTyInfo <- getTyInfo fldBaseTy

  argMap <- withPathK "args" $ processArgs (_fiParams fldInfo) args

  fields <- case (fldTyInfo, selSet) of

    (TIObj _, [])  ->
      throwVE $ "field " <> showName name <> " of type "
      <> G.showGT fldTy <> " must have a selection of subfields"

    (TIObj fldObjTyInfo, _) ->
      denormSelSet visFrags fldObjTyInfo selSet

    (TIScalar _, []) -> return Seq.empty
    (TIEnum _, []) -> return Seq.empty

    (TIInpObj _, _) ->
      throwVE $ "internal error: unexpected input type for field: "
      <> showName name

    (TIIFace _, []) ->
      throwVE $ "field " <> showName name <> " of type "
      <> G.showGT fldTy <> " must have a selection of subfields"

-- add some trivial logic for interface validation
    (TIIFace _, _) -> throwVE $ "interface types not supported"

    (TIUnion _, _) -> throwVE $ "union types not supported"

    -- when scalar/enum and no empty set
    (_, _) ->
      throwVE $ "field " <> showName name <> " must not have a "
      <> "selection since type " <> G.showGT fldTy <> " has no subfields"

  fldMap <- asks _vcFields
  let mtypedField = Map.lookup (_otiName parObjTyInfo, name) fldMap

  withPathK "directives" $ withDirectives dirs $ return $
    (Field (fromMaybe (G.Alias name) aliasM) name fldBaseTy argMap (fmap getLoc fields)
           (case mtypedField of
              Just (FldRemote remoteField) -> pure remoteField
              _                            -> Nothing))

denormInlnFrag
  :: ( MonadReader ValidationCtx m
     , MonadError QErr m)
  => [G.Name] -- visited fragments
  -> ObjTyInfo -- type information of the field
  -> G.InlineFragment
  -> m (Maybe FieldGroup)
denormInlnFrag visFrags fldTyInfo inlnFrag = do
  let fldTy  = _otiName fldTyInfo
  let fragTy = fromMaybe fldTy tyM
  when (fldTy /= fragTy) $
    throwVE $ "inline fragment is expected on type " <>
    showNamedTy fldTy <> " but found " <> showNamedTy fragTy
  withPathK "directives" $ withDirectives directives $
    fmap (FieldGroup FGSInlnFrag) $ denormSelSet visFrags fldTyInfo selSet
  where
    G.InlineFragment tyM directives selSet = inlnFrag

data Located a
  = HasuraLocated a
  | RemoteLocated RemoteSchemaInfo a
   deriving (Functor, Show, Eq, Traversable, Foldable)

getLoc :: Located a -> a
getLoc (HasuraLocated a)   = a
getLoc (RemoteLocated _ a) = a

denormSelSet
  :: ( MonadReader ValidationCtx m
     , MonadError QErr m)
  => [G.Name] -- visited fragments
  -> ObjTyInfo
  -> G.SelectionSet
  -> m (Seq.Seq (Located Field))
denormSelSet visFrags fldTyInfo selSet =
  withPathK "selectionSet" $ do
    resFlds <- catMaybes <$> mapM (denormSel visFrags fldTyInfo) selSet
    mergeFields $ foldl' flatten Seq.empty resFlds
  where
    flatten s (Left fld) = s Seq.|> fld
    flatten s (Right (FieldGroup _ flds)) =
      s Seq.>< flds

mergeFields
  :: ( MonadReader ValidationCtx m
     , MonadError QErr m)
  => Seq.Seq (Located Field)
  -> m (Seq.Seq (Located Field))
mergeFields flds =
  fmap Seq.fromList $ forM fldGroups $ \fieldGroup -> do
    newFld <- checkMergeability fieldGroup
    childFields <- mergeFields $ foldl' (\l f -> l Seq.>< traverse _fSelSet f) Seq.empty
                   $ NE.toSeq fieldGroup
    return $ fmap (\f -> f {_fSelSet = fmap getLoc childFields}) newFld
  where
    fldGroups = OMap.elems $ OMap.groupListWith (_fAlias . getLoc) flds
    -- can a group be merged?
    checkMergeability fldGroup = do
      let groupedFlds = toList $ NE.toSeq fldGroup
          fldNames = L.nub $ map (_fName . getLoc) groupedFlds
          args = L.nub $ map (_fArguments . getLoc) groupedFlds
          fld = NE.head fldGroup
          fldAl = _fAlias (getLoc fld)
      when (length fldNames > 1) $
        throwVE $ "cannot merge different fields under the same alias ("
        <> showName (G.unAlias fldAl) <> "): "
        <> showNames fldNames
      when (length args > 1) $
        throwVE $ "cannot merge fields with different arguments"
        <> " under the same alias: "
        <> showName (G.unAlias fldAl)
      return fld

denormFrag
  :: ( MonadReader ValidationCtx m
     , MonadError QErr m)
  => [G.Name] -- visited fragments
  -> G.NamedType -- parent type
  -> G.FragmentSpread
  -> m (Maybe FieldGroup)
denormFrag visFrags parTy (G.FragmentSpread name directives) = do

  -- check for cycles
  when (name `elem` visFrags) $
    throwVE $ "cannot spread fragment " <> showName name
    <> " within itself via "
    <> T.intercalate "," (map G.unName visFrags)

  (FragDef _ fragTyInfo selSet) <- getFragInfo

  let fragTy = _otiName fragTyInfo

  -- we don't have unions or interfaces so we can get away with equality
  when (fragTy /= parTy) $
    throwVE $ "cannot spread fragment " <> showName name <> " defined on " <>
    showNamedTy fragTy <> " when selecting fields of type " <> showNamedTy parTy

  resFlds <- denormSelSet (name:visFrags) fragTyInfo selSet

  withPathK "directives" $ withDirectives directives $
    return $ FieldGroup (FGSFragSprd  name) resFlds

  where
    getFragInfo = do
      dctx <- ask
      onNothing (Map.lookup name $ _vcFragDefMap dctx) $
        throwVE $ "fragment '" <> G.unName name <> "' not found"
