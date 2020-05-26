{-# LANGUAGE GADTs #-}
module Hasura.GraphQL.Validate.Types
  ( InpValInfo(..)
  , ParamMap

  , typenameFld
  , ObjFldInfo(..)
  , mkHsraObjFldInfo
  , ObjFieldMap

  -- Don't expose 'ObjTyInfo' constructor. Instead use 'mkObjTyInfo' or 'mkHsraObjTyInfo'
  -- which will auto-insert the compulsory '__typename' field.
  , ObjTyInfo
  , _otiDesc
  , _otiName
  , _otiImplIFaces
  , _otiFields
  , mkObjTyInfo
  , mkHsraObjTyInfo

  , IFaceTyInfo(..)
  , IFacesSet
  , UnionTyInfo(..)
  , FragDef(..)
  , FragmentTypeInfo(..)
  , FragDefMap
  , AnnVarVals
  , AnnInpVal(..)

  , EnumTyInfo(..)
  , mkHsraEnumTyInfo

  , EnumValuesInfo(..)
  , normalizeEnumValues
  , EnumValInfo(..)
  , InpObjFldMap
  , InpObjTyInfo(..)
  , mkHsraInpTyInfo

  , ScalarTyInfo(..)
  , fromScalarTyDef
  , mkHsraScalarTyInfo

  , DirectiveInfo(..)
  , AsObjType(..)
  , defaultDirectives
  , defDirectivesMap
  , defaultSchema
  , TypeInfo(..)
  , isObjTy
  , isIFaceTy
  , getPossibleObjTypes
  , getObjTyM
  , getUnionTyM
  , mkScalarTy
  , pgColTyToScalar
  , getNamedTy
  , mkTyInfoMap
  , fromTyDef
  , fromSchemaDoc
  , fromSchemaDocQ
  , TypeMap
  , TypeLoc (..)
  , typeEq
  , AnnGValue(..)
  , AnnGEnumValue(..)
  , AnnGObject
  , hasNullVal
  , getAnnInpValKind
  , stripTypenames

  , ReusableVariableTypes(..)
  , ReusableVariableValues

  , QueryReusability(..)
  , _Reusable
  , _NotReusable
  , MonadReusability(..)
  , ReusabilityT
  , runReusabilityT
  , runReusabilityTWith
  , evalReusabilityT

  , module Hasura.GraphQL.Utils
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Casing             as J
import qualified Data.Aeson.TH                 as J
import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G
import qualified Language.GraphQL.Draft.TH     as G
import qualified Language.Haskell.TH.Syntax    as TH

import           Control.Lens                  (makePrisms)

import qualified Hasura.RQL.Types.Column       as RQL

import           Hasura.GraphQL.NormalForm
import           Hasura.GraphQL.Utils
import           Hasura.RQL.Instances          ()
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.SQL.Types
import           Hasura.SQL.Value

-- | Typeclass for equating relevant properties of various GraphQL types defined below
class EquatableGType a where
  type EqProps a
  getEqProps :: a -> EqProps a

typeEq :: (EquatableGType a, Eq (EqProps a)) => a -> a -> Bool
typeEq a b = getEqProps a == getEqProps b

data EnumValInfo
  = EnumValInfo
  { _eviDesc         :: !(Maybe G.Description)
  , _eviVal          :: !G.EnumValue
  , _eviIsDeprecated :: !Bool
  } deriving (Show, Eq, TH.Lift)

fromEnumValDef :: G.EnumValueDefinition -> EnumValInfo
fromEnumValDef (G.EnumValueDefinition descM val _) =
  EnumValInfo descM val False

data EnumValuesInfo
  = EnumValuesSynthetic !(Map.HashMap G.EnumValue EnumValInfo)
  -- ^ Values for an enum that exists only in the GraphQL schema and does not
  -- have any external source of truth.
  | EnumValuesReference !RQL.EnumReference
  -- ^ Values for an enum that is backed by an enum table reference (see
  -- "Hasura.RQL.Schema.Enum").
  deriving (Show, Eq, TH.Lift)

normalizeEnumValues :: EnumValuesInfo -> Map.HashMap G.EnumValue EnumValInfo
normalizeEnumValues = \case
  EnumValuesSynthetic values -> values
  EnumValuesReference (RQL.EnumReference _ values) ->
    mapFromL _eviVal . flip map (Map.toList values) $
      \(RQL.EnumValue name, RQL.EnumValueInfo maybeDescription) -> EnumValInfo
        { _eviVal = G.EnumValue $ G.Name name
        , _eviDesc = G.Description <$> maybeDescription
        , _eviIsDeprecated = False }

data EnumTyInfo
  = EnumTyInfo
  { _etiDesc   :: !(Maybe G.Description)
  , _etiName   :: !G.NamedType
  , _etiValues :: !EnumValuesInfo
  , _etiLoc    :: !TypeLoc
  } deriving (Show, Eq, TH.Lift)

instance EquatableGType EnumTyInfo where
  type EqProps EnumTyInfo = (G.NamedType, Map.HashMap G.EnumValue EnumValInfo)
  getEqProps ety = (,) (_etiName ety) (normalizeEnumValues $ _etiValues ety)

fromEnumTyDef :: G.EnumTypeDefinition -> TypeLoc -> EnumTyInfo
fromEnumTyDef (G.EnumTypeDefinition descM n _ valDefs) loc =
  EnumTyInfo descM (G.NamedType n) (EnumValuesSynthetic enumVals) loc
  where
    enumVals = Map.fromList
      [(G._evdName valDef, fromEnumValDef valDef) | valDef <- valDefs]

mkHsraEnumTyInfo
  :: Maybe G.Description
  -> G.NamedType
  -> EnumValuesInfo
  -> EnumTyInfo
mkHsraEnumTyInfo descM ty enumVals =
  EnumTyInfo descM ty enumVals TLHasuraType

data InpValInfo
  = InpValInfo
  { _iviDesc   :: !(Maybe G.Description)
  , _iviName   :: !G.Name
  , _iviDefVal :: !(Maybe G.ValueConst)
  , _iviType   :: !G.GType
  } deriving (Show, Eq, TH.Lift)

instance EquatableGType InpValInfo where
  type EqProps InpValInfo = (G.Name, G.GType)
  getEqProps ity = (,) (_iviName ity) (_iviType ity)

fromInpValDef :: G.InputValueDefinition -> InpValInfo
fromInpValDef (G.InputValueDefinition descM n ty defM) =
  InpValInfo descM n defM ty

type ParamMap = Map.HashMap G.Name InpValInfo

-- | location of the type: a hasura type or a remote type
data TypeLoc
  = TLHasuraType
  | TLRemoteType !RemoteSchemaName !RemoteSchemaInfo
  | TLCustom
  deriving (Show, Eq, TH.Lift, Generic)

$(J.deriveJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase . drop 2
                   , J.sumEncoding = J.TaggedObject "type" "detail"
                   }
  ''TypeLoc)

instance Hashable TypeLoc

data ObjFldInfo
  = ObjFldInfo
  { _fiDesc   :: !(Maybe G.Description)
  , _fiName   :: !G.Name
  , _fiParams :: !ParamMap
  , _fiTy     :: !G.GType
  , _fiLoc    :: !TypeLoc
  } deriving (Show, Eq, TH.Lift)

instance EquatableGType ObjFldInfo where
  type EqProps ObjFldInfo = (G.Name, G.GType, ParamMap)
  getEqProps o = (,,) (_fiName o) (_fiTy o) (_fiParams o)

fromFldDef :: G.FieldDefinition -> TypeLoc -> ObjFldInfo
fromFldDef (G.FieldDefinition descM n args ty _) loc =
  ObjFldInfo descM n params ty loc
  where
    params = Map.fromList [(G._ivdName arg, fromInpValDef arg) | arg <- args]

mkHsraObjFldInfo
  :: Maybe G.Description
  -> G.Name
  -> ParamMap
  -> G.GType
  -> ObjFldInfo
mkHsraObjFldInfo descM name params ty =
  ObjFldInfo descM name params ty TLHasuraType

type ObjFieldMap = Map.HashMap G.Name ObjFldInfo

type IFacesSet = Set.HashSet G.NamedType

data ObjTyInfo
  = ObjTyInfo
  { _otiDesc       :: !(Maybe G.Description)
  , _otiName       :: !G.NamedType
  , _otiImplIFaces :: !IFacesSet
  , _otiFields     :: !ObjFieldMap
  } deriving (Show, Eq, TH.Lift)

instance EquatableGType ObjTyInfo where
  type EqProps ObjTyInfo =
    (G.NamedType, Set.HashSet G.NamedType,  Map.HashMap G.Name (G.Name, G.GType, ParamMap))
  getEqProps a = (,,) (_otiName a) (_otiImplIFaces a) (Map.map getEqProps (_otiFields a))

instance Monoid ObjTyInfo where
  mempty = ObjTyInfo Nothing (G.NamedType "") Set.empty Map.empty

instance Semigroup ObjTyInfo where
  objA <> objB =
    objA { _otiFields = Map.union (_otiFields objA) (_otiFields objB)
         , _otiImplIFaces = _otiImplIFaces objA `Set.union` _otiImplIFaces objB
         }

mkObjTyInfo
  :: Maybe G.Description -> G.NamedType
  -> IFacesSet -> ObjFieldMap -> TypeLoc -> ObjTyInfo
mkObjTyInfo descM ty iFaces flds _ =
  ObjTyInfo descM ty iFaces $ Map.insert (_fiName newFld) newFld flds
  where newFld = typenameFld

mkHsraObjTyInfo
  :: Maybe G.Description
  -> G.NamedType
  -> IFacesSet
  -> ObjFieldMap
  -> ObjTyInfo
mkHsraObjTyInfo descM ty implIFaces flds =
  mkObjTyInfo descM ty implIFaces flds TLHasuraType

mkIFaceTyInfo
  :: Maybe G.Description -> G.NamedType
  -> Map.HashMap G.Name ObjFldInfo -> TypeLoc -> MemberTypes -> IFaceTyInfo
mkIFaceTyInfo descM ty flds _ =
  IFaceTyInfo descM ty $ Map.insert (_fiName newFld) newFld flds
  where
    newFld = typenameFld

typenameFld :: ObjFldInfo
typenameFld =
  ObjFldInfo (Just desc) "__typename" Map.empty
    (G.toGT $ G.toNT $ G.NamedType "String") TLHasuraType
  where
    desc = "The name of the current Object type at runtime"

fromObjTyDef :: G.ObjectTypeDefinition -> TypeLoc -> ObjTyInfo
fromObjTyDef (G.ObjectTypeDefinition descM n ifaces _ flds) loc =
  mkObjTyInfo descM (G.NamedType n) (Set.fromList ifaces) fldMap loc
  where
    fldMap = Map.fromList [(G._fldName fld, fromFldDef fld loc) | fld <- flds]

data IFaceTyInfo
  = IFaceTyInfo
  { _ifDesc        :: !(Maybe G.Description)
  , _ifName        :: !G.NamedType
  , _ifFields      :: !ObjFieldMap
  , _ifMemberTypes :: !MemberTypes
  } deriving (Show, Eq, TH.Lift)

instance EquatableGType IFaceTyInfo where
  type EqProps IFaceTyInfo =
    (G.NamedType, Map.HashMap G.Name (G.Name, G.GType, ParamMap))
  getEqProps a = (,) (_ifName a) (Map.map getEqProps (_ifFields a))

instance Semigroup IFaceTyInfo where
  objA <> objB =
    objA { _ifFields = Map.union (_ifFields objA) (_ifFields objB)
         }

fromIFaceDef
  :: InterfaceImplementations -> G.InterfaceTypeDefinition -> TypeLoc -> IFaceTyInfo
fromIFaceDef interfaceImplementations (G.InterfaceTypeDefinition descM n _ flds) loc =
  mkIFaceTyInfo descM (G.NamedType n) fldMap loc implementations
  where
    fldMap = Map.fromList [(G._fldName fld, fromFldDef fld loc) | fld <- flds]
    implementations = fromMaybe mempty $ Map.lookup (G.NamedType n) interfaceImplementations

type MemberTypes = Set.HashSet G.NamedType

data UnionTyInfo
  = UnionTyInfo
  { _utiDesc        :: !(Maybe G.Description)
  , _utiName        :: !G.NamedType
  , _utiMemberTypes :: !MemberTypes
  } deriving (Show, Eq, TH.Lift)

instance EquatableGType UnionTyInfo where
  type EqProps UnionTyInfo =
    (G.NamedType, Set.HashSet G.NamedType)
  getEqProps a = (,) (_utiName a) (_utiMemberTypes a)

instance Monoid UnionTyInfo where
  mempty = UnionTyInfo Nothing (G.NamedType "") Set.empty

instance Semigroup UnionTyInfo where
  objA <> objB =
    objA { _utiMemberTypes = Set.union (_utiMemberTypes objA) (_utiMemberTypes objB)
         }

fromUnionTyDef :: G.UnionTypeDefinition -> UnionTyInfo
fromUnionTyDef (G.UnionTypeDefinition descM n _ mt) = UnionTyInfo descM (G.NamedType n) $ Set.fromList mt

type InpObjFldMap = Map.HashMap G.Name InpValInfo

data InpObjTyInfo
  = InpObjTyInfo
  { _iotiDesc   :: !(Maybe G.Description)
  , _iotiName   :: !G.NamedType
  , _iotiFields :: !InpObjFldMap
  , _iotiLoc    :: !TypeLoc
  } deriving (Show, Eq, TH.Lift)

instance EquatableGType InpObjTyInfo where
  type EqProps InpObjTyInfo = (G.NamedType, Map.HashMap G.Name (G.Name, G.GType))
  getEqProps a = (,) (_iotiName a) (Map.map getEqProps $ _iotiFields a)

fromInpObjTyDef :: G.InputObjectTypeDefinition -> TypeLoc -> InpObjTyInfo
fromInpObjTyDef (G.InputObjectTypeDefinition descM n _ inpFlds) loc =
  InpObjTyInfo descM (G.NamedType n) fldMap loc
  where
    fldMap = Map.fromList
      [(G._ivdName inpFld, fromInpValDef inpFld) | inpFld <- inpFlds]

mkHsraInpTyInfo
  :: Maybe G.Description
  -> G.NamedType
  -> InpObjFldMap
  -> InpObjTyInfo
mkHsraInpTyInfo descM ty flds =
  InpObjTyInfo descM ty flds TLHasuraType

data ScalarTyInfo
  = ScalarTyInfo
  { _stiDesc :: !(Maybe G.Description)
  , _stiName :: !G.Name
  , _stiType :: !PGScalarType
  , _stiLoc  :: !TypeLoc
  } deriving (Show, Eq, TH.Lift)

mkHsraScalarTyInfo :: PGScalarType -> ScalarTyInfo
mkHsraScalarTyInfo ty =
  ScalarTyInfo Nothing (G.Name $ pgColTyToScalar ty) ty TLHasuraType

instance EquatableGType ScalarTyInfo where
  type EqProps ScalarTyInfo = PGScalarType
  getEqProps = _stiType

fromScalarTyDef
  :: G.ScalarTypeDefinition
  -> TypeLoc
  -> ScalarTyInfo
fromScalarTyDef (G.ScalarTypeDefinition descM n _) =
  ScalarTyInfo descM n ty
  where
    ty = case n of
      "Int"     -> PGInteger
      "Float"   -> PGFloat
      "String"  -> PGText
      "Boolean" -> PGBoolean
      "ID"      -> PGText
      _         -> textToPGScalarType $ G.unName n

data TypeInfo
  = TIScalar !ScalarTyInfo
  | TIObj !ObjTyInfo
  | TIEnum !EnumTyInfo
  | TIInpObj !InpObjTyInfo
  | TIIFace !IFaceTyInfo
  | TIUnion !UnionTyInfo
  deriving (Show, Eq, TH.Lift)

instance J.ToJSON TypeInfo where
  toJSON _ = J.String "toJSON not implemented for TypeInfo"

data AsObjType
  = AOTIFace IFaceTyInfo
  | AOTUnion UnionTyInfo

getPossibleObjTypes :: TypeMap -> AsObjType -> Map.HashMap G.NamedType ObjTyInfo
getPossibleObjTypes tyMap = \case
  (AOTIFace i) ->
    toObjMap $ mapMaybe (extrObjTyInfoM tyMap) $ Set.toList $ _ifMemberTypes i
  (AOTUnion u) ->
    toObjMap $ mapMaybe (extrObjTyInfoM tyMap) $ Set.toList $ _utiMemberTypes u
  -- toObjMap $ mapMaybe previewImplTypeM $ Map.elems tyMap
  -- where
  --   previewImplTypeM = \case
  --     TIObj objTyInfo -> bool Nothing (Just objTyInfo) $
  --        _ifName i `elem` _otiImplIFaces objTyInfo
  --     _               -> Nothing


toObjMap :: [ObjTyInfo] -> Map.HashMap G.NamedType ObjTyInfo
toObjMap objs = foldr (\o -> Map.insert (_otiName o) o) Map.empty objs


isObjTy :: TypeInfo -> Bool
isObjTy = \case
  (TIObj _) -> True
  _         -> False

getObjTyM :: TypeInfo -> Maybe ObjTyInfo
getObjTyM = \case
  (TIObj t) -> return t
  _         -> Nothing

getUnionTyM :: TypeInfo -> Maybe UnionTyInfo
getUnionTyM = \case
  (TIUnion u) -> return u
  _         -> Nothing

isIFaceTy :: TypeInfo -> Bool
isIFaceTy = \case
  (TIIFace _) -> True
  _         -> False

data SchemaPath
  = SchemaPath
  { _spTypeName :: !(Maybe G.NamedType)
  , _spFldName  :: !(Maybe G.Name)
  , _spArgName  :: !(Maybe G.Name)
  , _spType     :: !(Maybe T.Text)
  }

setFldNameSP :: SchemaPath -> G.Name -> SchemaPath
setFldNameSP sp fn = sp { _spFldName = Just fn}

setArgNameSP :: SchemaPath -> G.Name -> SchemaPath
setArgNameSP sp an = sp { _spArgName = Just an}

showSP :: SchemaPath -> Text
showSP (SchemaPath t f a _) = maybe "" (\x -> showNamedTy x <> fN) t
  where
    fN = maybe "" (\x -> "." <> showName x <> aN) f
    aN = maybe "" showArg a
    showArg x = "(" <> showName x <> ":)"

showSPTxt' :: SchemaPath -> Text
showSPTxt' (SchemaPath _ f a t)  = maybe "" (<> " "<> fld) t
  where
    fld = maybe "" (const $ "field " <> arg) f
    arg = maybe "" (const "argument ") a

showSPTxt :: SchemaPath -> Text
showSPTxt p = showSPTxt' p <> showSP p

validateIFace :: MonadError Text f => IFaceTyInfo -> f ()
validateIFace (IFaceTyInfo _ n flds _) =
  when (isFldListEmpty flds) $ throwError $ "List of fields cannot be empty for interface " <> showNamedTy n

validateObj :: TypeMap -> ObjTyInfo -> Either Text ()
validateObj tyMap objTyInfo@(ObjTyInfo _ n _ flds) = do
  when (isFldListEmpty flds) $ throwError $ "List of fields cannot be empty for " <> objTxt
  mapM_ (extrIFaceTyInfo' >=> validateIFaceImpl objTyInfo) $ _otiImplIFaces objTyInfo
  where
    extrIFaceTyInfo' t = withObjTxt $ extrIFaceTyInfo tyMap t
    withObjTxt x = x `catchError` \e -> throwError $ e <> " implemented by " <> objTxt
    objTxt = "Object type " <> showNamedTy n
    validateIFaceImpl = implmntsIFace tyMap

isFldListEmpty :: ObjFieldMap -> Bool
isFldListEmpty = Map.null . Map.delete "__typename"

validateUnion :: MonadError Text m => TypeMap -> UnionTyInfo -> m ()
validateUnion tyMap (UnionTyInfo _ un mt) = do
  when (Set.null mt) $ throwError $ "List of member types cannot be empty for union type " <> showNamedTy un
  mapM_ valIsObjTy $ Set.toList mt
  where
    valIsObjTy mn = case Map.lookup mn tyMap of
      Just (TIObj t) -> return t
      Nothing -> throwError $ "Could not find type " <> showNamedTy mn <> ", which is defined as a member type of Union " <> showNamedTy un
      _ -> throwError $ "Union type " <> showNamedTy un <> " can only include object types. It cannot include " <> showNamedTy mn

implmntsIFace :: TypeMap -> ObjTyInfo -> IFaceTyInfo -> Either Text ()
implmntsIFace tyMap objTyInfo iFaceTyInfo = do
  let path =
        ( SchemaPath (Just $ _otiName objTyInfo) Nothing Nothing (Just "Object")
        , SchemaPath  (Just $ _ifName iFaceTyInfo) Nothing Nothing (Just "Interface")
        )
  mapM_ (includesIFaceFld path) $ _ifFields iFaceTyInfo
  where
    includesIFaceFld (spO,spIF) ifFld = do
      let pathA@(spOA, spIFA) = (spO, setFldNameSP spIF $ _fiName ifFld)
      objFld <- sameNameFld pathA ifFld
      let pathB = (setFldNameSP spOA $ _fiName objFld, spIFA)
      validateIsSubType' pathB (_fiTy objFld) (_fiTy ifFld)
      hasAllArgs pathB objFld ifFld
      isExtraArgsNullable pathB objFld ifFld

    validateIsSubType' (spO,spIF) oFld iFld = validateIsSubType tyMap oFld iFld `catchError` \_ ->
      throwError $ "The type of " <> showSPTxt spO <> " (" <> G.showGT oFld <>
      ") is not the same type/sub type of " <> showSPTxt spIF <> " (" <> G.showGT iFld <> ")"

    sameNameFld (spO, spIF) ifFld = do
      let spIFN = setFldNameSP spIF $ _fiName ifFld
      onNothing (Map.lookup (_fiName ifFld) objFlds)
        $ throwError $ showSPTxt spIFN <> " expected, but " <> showSP spO <> " does not provide it"

    hasAllArgs (spO, spIF) objFld ifFld = forM_ (_fiParams ifFld) $ \ifArg -> do
      objArg <- sameNameArg ifArg
      let (spON, spIFN) = (setArgNameSP spO $ _iviName objArg, setArgNameSP spIF $ _iviName ifArg)
      unless (_iviType objArg == _iviType ifArg) $ throwError $
        showSPTxt spIFN <> " expects type " <> G.showGT (_iviType ifArg) <> ", but " <>
        showSP spON <> " has type " <> G.showGT (_iviType objArg)
      where
        sameNameArg ivi = do
          let spIFN = setArgNameSP spIF $ _iviName ivi
          onNothing (Map.lookup (_iviName ivi) objArgs) $ throwError $ showSPTxt spIFN <> " required, but " <>
            showSPTxt spO <> " does not provide it"
        objArgs = _fiParams objFld

    isExtraArgsNullable (spO, spIF) objFld ifFld = forM_ extraArgs isInpValNullable
      where
        extraArgs = Map.difference (_fiParams objFld) (_fiParams ifFld)
        isInpValNullable ivi = unless (G.isNullable $ _iviType ivi) $ throwError $
          showSPTxt (setArgNameSP spO $ _iviName ivi) <> " is of required type "
          <> G.showGT (_iviType ivi) <> ", but is not provided by " <> showSPTxt spIF

    objFlds =  _otiFields objTyInfo

extrTyInfo :: TypeMap -> G.NamedType -> Either Text TypeInfo
extrTyInfo tyMap tn = maybe
  (throwError $ "Could not find type with name " <> showNamedTy tn)
  return
  $ Map.lookup tn tyMap

extrIFaceTyInfo :: MonadError Text m => Map.HashMap G.NamedType TypeInfo -> G.NamedType -> m IFaceTyInfo
extrIFaceTyInfo tyMap tn = case Map.lookup tn tyMap of
  Just (TIIFace i) -> return i
  _                -> throwError $ "Could not find interface " <> showNamedTy tn

extrObjTyInfoM :: TypeMap -> G.NamedType -> Maybe ObjTyInfo
extrObjTyInfoM tyMap tn = case Map.lookup tn tyMap of
  Just (TIObj o) -> return o
  _              -> Nothing

validateIsSubType :: Map.HashMap G.NamedType TypeInfo -> G.GType -> G.GType -> Either Text ()
validateIsSubType tyMap subFldTy supFldTy = do
  checkNullMismatch subFldTy supFldTy
  case (subFldTy,supFldTy) of
    (G.TypeNamed _ subTy, G.TypeNamed _ supTy) -> do
      subTyInfo <- extrTyInfo tyMap subTy
      supTyInfo <- extrTyInfo tyMap supTy
      isSubTypeBase subTyInfo supTyInfo
    (G.TypeList _ (G.ListType sub), G.TypeList _ (G.ListType sup) ) ->
      validateIsSubType tyMap sub sup
    _ -> throwError $ showIsListTy subFldTy <> " Type " <> G.showGT subFldTy <>
      " cannot be a sub-type of " <> showIsListTy supFldTy <> " Type " <> G.showGT supFldTy
  where
    checkNullMismatch subTy supTy = when (G.isNotNull supTy && G.isNullable subTy ) $
      throwError $ "Nullable Type " <> G.showGT subFldTy <> " cannot be a sub-type of Non-Null Type " <> G.showGT supFldTy
    showIsListTy = \case
      G.TypeList  {} -> "List"
      G.TypeNamed {} -> "Named"

-- TODO Should we check the schema location as well?
isSubTypeBase :: (MonadError Text m) => TypeInfo -> TypeInfo -> m ()
isSubTypeBase subTyInfo supTyInfo = case (subTyInfo,supTyInfo) of
  (TIObj obj, TIIFace iFace) -> unless (_ifName iFace `elem` _otiImplIFaces obj) notSubTyErr
  _                          -> unless (subTyInfo == supTyInfo) notSubTyErr
  where
    showTy = showNamedTy . getNamedTy
    notSubTyErr = throwError $ "Type " <> showTy subTyInfo <> " is not a sub type of " <> showTy supTyInfo

-- map postgres types to builtin scalars
pgColTyToScalar :: PGScalarType -> Text
pgColTyToScalar = \case
  PGInteger -> "Int"
  PGBoolean -> "Boolean"
  PGFloat   -> "Float"
  PGText    -> "String"
  PGVarchar -> "String"
  t         -> toSQLTxt t

mkScalarTy :: PGScalarType -> G.NamedType
mkScalarTy =
  G.NamedType . G.Name . pgColTyToScalar

getNamedTy :: TypeInfo -> G.NamedType
getNamedTy = \case
  TIScalar t -> G.NamedType $ _stiName t
  TIObj t -> _otiName t
  TIIFace i -> _ifName i
  TIEnum t -> _etiName t
  TIInpObj t -> _iotiName t
  TIUnion u -> _utiName u

mkTyInfoMap :: [TypeInfo] -> TypeMap
mkTyInfoMap tyInfos =
  Map.fromList [(getNamedTy tyInfo, tyInfo) | tyInfo <- tyInfos]

fromTyDef :: InterfaceImplementations -> TypeLoc -> G.TypeDefinition -> TypeInfo
fromTyDef interfaceImplementations loc tyDef = case tyDef of
  G.TypeDefinitionScalar t      -> TIScalar $ fromScalarTyDef t loc
  G.TypeDefinitionObject t      -> TIObj $ fromObjTyDef t loc
  G.TypeDefinitionInterface t   -> TIIFace $ fromIFaceDef interfaceImplementations t loc
  G.TypeDefinitionUnion t       -> TIUnion $ fromUnionTyDef t
  G.TypeDefinitionEnum t        -> TIEnum $ fromEnumTyDef t loc
  G.TypeDefinitionInputObject t -> TIInpObj $ fromInpObjTyDef t loc

type InterfaceImplementations = Map.HashMap G.NamedType MemberTypes

fromSchemaDoc :: G.SchemaDocument -> TypeLoc -> Either Text TypeMap
fromSchemaDoc (G.SchemaDocument tyDefs) loc = do
  let tyMap = mkTyInfoMap $ map (fromTyDef interfaceImplementations loc) tyDefs
  validateTypeMap tyMap
  return tyMap
  where
    interfaceImplementations :: InterfaceImplementations
    interfaceImplementations =
      foldr (Map.unionWith (<>)) mempty $ flip mapMaybe tyDefs $ \case
        G.TypeDefinitionObject objectDefinition ->
          Just $ Map.fromList $ zip
            (G._otdImplementsInterfaces objectDefinition)
            (repeat $ Set.singleton $ G.NamedType $ G._otdName objectDefinition)
        _ -> Nothing

validateTypeMap :: TypeMap -> Either Text ()
validateTypeMap tyMap =  mapM_ validateTy $ Map.elems tyMap
  where
    validateTy (TIObj o)   = validateObj tyMap o
    validateTy (TIUnion u) = validateUnion tyMap u
    validateTy (TIIFace i) = validateIFace i
    validateTy _           = return ()

fromSchemaDocQ :: G.SchemaDocument -> TypeLoc -> TH.Q TH.Exp
fromSchemaDocQ sd loc = case fromSchemaDoc sd loc of
  Left e      -> fail $ T.unpack e
  Right tyMap -> TH.ListE <$> mapM TH.lift (Map.elems tyMap)

defaultSchema :: G.SchemaDocument
defaultSchema = $(G.parseSchemaDocQ "src-rsr/schema.graphql")

-- fromBaseSchemaFileQ :: FilePath -> TH.Q TH.Exp
-- fromBaseSchemaFileQ fp =
--   fromSchemaDocQ $(G.parseSchemaDocQ fp)

type TypeMap = Map.HashMap G.NamedType TypeInfo

data DirectiveInfo
  = DirectiveInfo
  { _diDescription :: !(Maybe G.Description)
  , _diName        :: !G.Name
  , _diParams      :: !ParamMap
  , _diLocations   :: ![G.DirectiveLocation]
  } deriving (Show, Eq)

-- TODO: generate this from template haskell once we have a parser for directive defs
-- directive @skip(if: Boolean!) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT
defaultDirectives :: [DirectiveInfo]
defaultDirectives =
  [mkDirective "skip", mkDirective "include"]
  where
    mkDirective n = DirectiveInfo Nothing n args dirLocs
    args = Map.singleton "if" $ InpValInfo Nothing "if" Nothing $
           G.TypeNamed (G.Nullability False) $ mkScalarTy PGBoolean
    dirLocs = map G.DLExecutable
              [G.EDLFIELD, G.EDLFRAGMENT_SPREAD, G.EDLINLINE_FRAGMENT]

defDirectivesMap :: Map.HashMap G.Name DirectiveInfo
defDirectivesMap = mapFromL _diName defaultDirectives

data FragDef
  = FragDef
  { _fdName   :: !G.Name
  , _fdTyInfo :: !FragmentTypeInfo
  , _fdSelSet :: !G.SelectionSet
  } deriving (Show, Eq)

data FragmentTypeInfo
  = FragmentTyObject !ObjTyInfo
  | FragmentTyInterface !IFaceTyInfo
  | FragmentTyUnion !UnionTyInfo
  deriving (Show, Eq)

type FragDefMap = Map.HashMap G.Name FragDef

type AnnVarVals =
  Map.HashMap G.Variable AnnInpVal

stripTypenames :: [G.ExecutableDefinition] -> [G.ExecutableDefinition]
stripTypenames = map filterExecDef
  where
    filterExecDef = \case
      G.ExecutableDefinitionOperation opDef  ->
        G.ExecutableDefinitionOperation $ filterOpDef opDef
      G.ExecutableDefinitionFragment fragDef ->
        let newSelset = filterSelSet $ G._fdSelectionSet fragDef
        in G.ExecutableDefinitionFragment fragDef{G._fdSelectionSet = newSelset}

    filterOpDef  = \case
      G.OperationDefinitionTyped typeOpDef ->
        let newSelset = filterSelSet $ G._todSelectionSet typeOpDef
        in G.OperationDefinitionTyped typeOpDef{G._todSelectionSet = newSelset}
      G.OperationDefinitionUnTyped selset ->
        G.OperationDefinitionUnTyped $ filterSelSet selset

    filterSelSet = mapMaybe filterSel
    filterSel s = case s of
      G.SelectionField f ->
        if G._fName f == "__typename"
        then Nothing
        else
          let newSelset = filterSelSet $ G._fSelectionSet f
          in Just $ G.SelectionField  f{G._fSelectionSet = newSelset}
      _                  -> Just s

-- | Used by 'Hasura.GraphQL.Validate.validateVariablesForReuse' to parse new sets of variables for
-- reusable query plans; see also 'QueryReusability'.
newtype ReusableVariableTypes
  = ReusableVariableTypes { unReusableVarTypes :: Map.HashMap G.Variable RQL.PGColumnType }
  deriving (Show, Eq, Semigroup, Monoid, J.ToJSON)
type ReusableVariableValues = Map.HashMap G.Variable (WithScalarType PGScalarValue)

-- | Tracks whether or not a query is /reusable/. Reusable queries are nice, since we can cache
-- their resolved ASTs and avoid re-resolving them if we receive an identical query. However, we
-- can’t always safely reuse queries if they have variables, since some variable values can affect
-- the generated SQL. For example, consider the following query:
--
-- > query users_where($condition: users_bool_exp!) {
-- >   users(where: $condition) {
-- >     id
-- >   }
-- > }
--
-- Different values for @$condition@ will produce completely different queries, so we can’t reuse
-- its plan (unless the variable values were also all identical, of course, but we don’t bother
-- caching those).
--
-- If a query does turn out to be reusable, we build up a 'ReusableVariableTypes' value that maps
-- variable names to their types so that we can use a fast path for validating new sets of
-- variables (namely 'Hasura.GraphQL.Validate.validateVariablesForReuse').
data QueryReusability
  = Reusable !ReusableVariableTypes
  | NotReusable
  deriving (Show, Eq)
$(makePrisms ''QueryReusability)

instance Semigroup QueryReusability where
  Reusable a <> Reusable b = Reusable (a <> b)
  _          <> _          = NotReusable
instance Monoid QueryReusability where
  mempty = Reusable mempty

class (Monad m) => MonadReusability m where
  recordVariableUse :: G.Variable -> RQL.PGColumnType -> m ()
  markNotReusable :: m ()

instance (MonadReusability m) => MonadReusability (ReaderT r m) where
  recordVariableUse a b = lift $ recordVariableUse a b
  markNotReusable = lift markNotReusable

instance (MonadReusability m) => MonadReusability (StateT s m) where
  recordVariableUse a b = lift $ recordVariableUse a b
  markNotReusable = lift markNotReusable

newtype ReusabilityT m a = ReusabilityT { unReusabilityT :: StateT QueryReusability m a }
  deriving (Functor, Applicative, Monad, MonadError e, MonadReader r, MonadIO)

instance (Monad m) => MonadReusability (ReusabilityT m) where
  recordVariableUse varName varType = ReusabilityT $
    modify' (<> Reusable (ReusableVariableTypes $ Map.singleton varName varType))
  markNotReusable = ReusabilityT $ put NotReusable

runReusabilityT :: ReusabilityT m a -> m (a, QueryReusability)
runReusabilityT = runReusabilityTWith mempty

-- | Like 'runReusabilityT', but starting from an existing 'QueryReusability' state.
runReusabilityTWith :: QueryReusability -> ReusabilityT m a -> m (a, QueryReusability)
runReusabilityTWith initialReusability = flip runStateT initialReusability . unReusabilityT

evalReusabilityT :: (Monad m) => ReusabilityT m a -> m a
evalReusabilityT = flip evalStateT mempty . unReusabilityT
