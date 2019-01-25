module Hasura.GraphQL.Validate.Types
  ( InpValInfo(..)
  , ParamMap
  , ObjFldInfo(..)
  , ObjFieldMap
  , ObjTyInfo(..)
  , mkObjTyInfo
  , FragDef(..)
  , FragDefMap
  , AnnVarVals
  , EnumTyInfo(..)
  , EnumValInfo(..)
  , InpObjFldMap
  , InpObjTyInfo(..)
  , ScalarTyInfo(..)
  , DirectiveInfo(..)
  , defaultDirectives
  , defDirectivesMap
  , defaultSchema
  , TypeInfo(..)
  , isObjTy
  , getObjTyM
  , mkScalarTy
  , pgColTyToScalar
  , pgColValToAnnGVal
  , getNamedTy
  , mkTyInfoMap
  , fromTyDef
  , fromTyDefQ
  , fromSchemaDocQ
  , TypeMap
  , TypeLoc (..)
  , typeEq
  , AnnGValue(..)
  , AnnGObject
  , hasNullVal
  , getAnnInpValKind
  , getAnnInpValTy
  , module Hasura.GraphQL.Utils
  ) where

import           Hasura.Prelude
import           Instances.TH.Lift             ()

import qualified Data.Aeson                    as J
import qualified Data.HashMap.Strict           as Map
import qualified Data.HashMap.Strict.InsOrd    as OMap
import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G
import qualified Language.GraphQL.Draft.TH     as G
import qualified Language.Haskell.TH.Syntax    as TH

import           Hasura.GraphQL.Utils
import           Hasura.RQL.Instances          ()
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.SQL.Types
import           Hasura.SQL.Value


-- | Typeclass for equating relevant properties of various GraphQL types
-- | defined below
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

data EnumTyInfo
  = EnumTyInfo
  { _etiDesc   :: !(Maybe G.Description)
  , _etiName   :: !G.NamedType
  , _etiValues :: !(Map.HashMap G.EnumValue EnumValInfo)
  , _etiLoc    :: !TypeLoc
  } deriving (Show, Eq, TH.Lift)

instance EquatableGType EnumTyInfo where
  type EqProps EnumTyInfo = (G.NamedType, Map.HashMap G.EnumValue EnumValInfo)
  getEqProps ety = (,) (_etiName ety) (_etiValues ety)

fromEnumTyDef :: G.EnumTypeDefinition -> TypeLoc -> EnumTyInfo
fromEnumTyDef (G.EnumTypeDefinition descM n _ valDefs) loc =
  EnumTyInfo descM (G.NamedType n) enumVals loc
  where
    enumVals = Map.fromList
      [(G._evdName valDef, fromEnumValDef valDef) | valDef <- valDefs]

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
  = HasuraType
  | RemoteType RemoteSchemaName RemoteSchemaInfo
  deriving (Show, Eq, TH.Lift, Generic)

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

type ObjFieldMap = Map.HashMap G.Name ObjFldInfo

data ObjTyInfo
  = ObjTyInfo
  { _otiDesc   :: !(Maybe G.Description)
  , _otiName   :: !G.NamedType
  , _otiFields :: !ObjFieldMap
  } deriving (Show, Eq, TH.Lift)

instance EquatableGType ObjTyInfo where
  type EqProps ObjTyInfo =
    (G.NamedType, Map.HashMap G.Name (G.Name, G.GType, ParamMap))
  getEqProps a = (,) (_otiName a) (Map.map getEqProps (_otiFields a))

instance Monoid ObjTyInfo where
  mempty = ObjTyInfo Nothing (G.NamedType "") Map.empty

instance Semigroup ObjTyInfo where
  objA <> objB =
    objA { _otiFields = Map.union (_otiFields objA) (_otiFields objB)
         }

mkObjTyInfo
  :: Maybe G.Description -> G.NamedType -> ObjFieldMap -> TypeLoc -> ObjTyInfo
mkObjTyInfo descM ty flds loc =
  ObjTyInfo descM ty $ Map.insert (_fiName newFld) newFld flds
  where newFld = typenameFld loc

typenameFld :: TypeLoc -> ObjFldInfo
typenameFld loc =
  ObjFldInfo (Just desc) "__typename" Map.empty
    (G.toGT $ G.toNT $ G.NamedType "String") loc
  where
    desc = "The name of the current Object type at runtime"

fromObjTyDef :: G.ObjectTypeDefinition -> TypeLoc -> ObjTyInfo
fromObjTyDef (G.ObjectTypeDefinition descM n _ _ flds) loc =
  mkObjTyInfo descM (G.NamedType n) fldMap loc
  where
    fldMap = Map.fromList [(G._fldName fld, fromFldDef fld loc) | fld <- flds]

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

data ScalarTyInfo
  = ScalarTyInfo
  { _stiDesc :: !(Maybe G.Description)
  , _stiType :: !PGColType
  , _stiLoc  :: !TypeLoc
  } deriving (Show, Eq, TH.Lift)

instance EquatableGType ScalarTyInfo where
  type EqProps ScalarTyInfo = PGColType
  getEqProps = _stiType

fromScalarTyDef
  :: G.ScalarTypeDefinition
  -> TypeLoc
  -> Either Text ScalarTyInfo
fromScalarTyDef (G.ScalarTypeDefinition descM n _) loc =
  ScalarTyInfo descM <$> ty <*> pure loc
  where
    ty = case n of
      "Int"     -> return PGInteger
      "Float"   -> return PGFloat
      "String"  -> return PGText
      "Boolean" -> return PGBoolean
      -- TODO: is this correct?
      "ID"      -> return $ PGUnknown "ID" --PGText
      -- FIXME: is this correct for hasura scalar also?
      _         -> return $ PGUnknown $ G.unName n --throwError $ "unexpected type: " <> G.unName n

data TypeInfo
  = TIScalar !ScalarTyInfo
  | TIObj !ObjTyInfo
  | TIEnum !EnumTyInfo
  | TIInpObj !InpObjTyInfo
  deriving (Show, Eq, TH.Lift)

isObjTy :: TypeInfo -> Bool
isObjTy = \case
  (TIObj _) -> True
  _         -> False

getObjTyM :: TypeInfo -> Maybe ObjTyInfo
getObjTyM = \case
  (TIObj t) -> return t
  _         -> Nothing

-- map postgres types to builtin scalars
pgColTyToScalar :: PGColType -> Text
pgColTyToScalar = \case
  PGInteger -> "Int"
  PGBoolean -> "Boolean"
  PGFloat   -> "Float"
  PGText    -> "String"
  PGVarchar -> "String"
  t         -> T.pack $ show t

mkScalarTy :: PGColType -> G.NamedType
mkScalarTy =
  G.NamedType . G.Name . pgColTyToScalar

getNamedTy :: TypeInfo -> G.NamedType
getNamedTy = \case
  TIScalar t -> mkScalarTy $ _stiType t
  TIObj t -> _otiName t
  TIEnum t -> _etiName t
  TIInpObj t -> _iotiName t

mkTyInfoMap :: [TypeInfo] -> TypeMap
mkTyInfoMap tyInfos =
  Map.fromList [(getNamedTy tyInfo, tyInfo) | tyInfo <- tyInfos]

fromTyDef :: G.TypeDefinition -> TypeLoc -> Either Text TypeInfo
fromTyDef tyDef loc = case tyDef of
  G.TypeDefinitionScalar t -> TIScalar <$> fromScalarTyDef t loc
  G.TypeDefinitionObject t -> return $ TIObj $ fromObjTyDef t loc
  G.TypeDefinitionInterface t ->
    throwError $ "unexpected interface: " <> showName (G._itdName t)
  G.TypeDefinitionUnion t ->
    throwError $ "unexpected union: " <> showName (G._utdName t)
  G.TypeDefinitionEnum t -> return $ TIEnum $ fromEnumTyDef t loc
  G.TypeDefinitionInputObject t -> return $ TIInpObj $ fromInpObjTyDef t loc

fromTyDefQ :: G.TypeDefinition -> TypeLoc -> TH.Q TH.Exp
fromTyDefQ tyDef loc = case fromTyDef tyDef loc of
  Left e  -> fail $ T.unpack e
  Right t -> TH.lift t

fromSchemaDocQ :: G.SchemaDocument -> TypeLoc -> TH.Q TH.Exp
fromSchemaDocQ (G.SchemaDocument tyDefs) loc =
  TH.ListE <$> mapM (flip fromTyDefQ loc) tyDefs

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
           G.TypeNamed (G.Nullability False) $ G.NamedType $ G.Name "Boolean"
    dirLocs = map G.DLExecutable
              [G.EDLFIELD, G.EDLFRAGMENT_SPREAD, G.EDLINLINE_FRAGMENT]

defDirectivesMap :: Map.HashMap G.Name DirectiveInfo
defDirectivesMap = mapFromL _diName defaultDirectives

data FragDef
  = FragDef
  { _fdName   :: !G.Name
  , _fdTyInfo :: !ObjTyInfo
  , _fdSelSet :: !G.SelectionSet
  } deriving (Show, Eq)

type FragDefMap = Map.HashMap G.Name FragDef

type AnnVarVals =
  Map.HashMap G.Variable AnnGValue

type AnnGObject = OMap.InsOrdHashMap G.Name AnnGValue

data AnnGValue
  = AGScalar !PGColType !(Maybe PGColValue)
  | AGEnum !G.NamedType !(Maybe G.EnumValue)
  | AGObject !G.NamedType !(Maybe AnnGObject)
  | AGArray !G.ListType !(Maybe [AnnGValue])
  deriving (Show, Eq)

instance J.ToJSON AnnGValue where
  -- toJSON (AGScalar ty valM) =
  toJSON = const J.Null
    -- J.
    -- J.toJSON [J.toJSON ty, J.toJSON valM]

pgColValToAnnGVal :: PGColType -> PGColValue -> AnnGValue
pgColValToAnnGVal colTy colVal = AGScalar colTy $ Just colVal

hasNullVal :: AnnGValue -> Bool
hasNullVal = \case
  AGScalar _ Nothing -> True
  AGEnum _ Nothing   -> True
  AGObject _ Nothing -> True
  AGArray _ Nothing  -> True
  _                  -> False

getAnnInpValKind :: AnnGValue -> Text
getAnnInpValKind = \case
  AGScalar _ _ -> "scalar"
  AGEnum _ _   -> "enum"
  AGObject _ _ -> "object"
  AGArray _ _  -> "array"

getAnnInpValTy :: AnnGValue -> G.GType
getAnnInpValTy = \case
  AGScalar pct _ -> G.TypeNamed (G.Nullability True) $ G.NamedType $ G.Name $ T.pack $ show pct
  AGEnum nt _    -> G.TypeNamed (G.Nullability True) nt
  AGObject nt _  -> G.TypeNamed (G.Nullability True) nt
  AGArray nt _   -> G.TypeList  (G.Nullability True) nt
