{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

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
  , AnnGValue(..)
  , AnnGObject
  , hasNullVal
  , getAnnInpValKind
  , getAnnInpValTy
  , module Hasura.GraphQL.Utils
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                    as J
import qualified Data.HashMap.Strict           as Map
import qualified Data.HashMap.Strict.InsOrd    as OMap
import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G
import qualified Language.GraphQL.Draft.TH     as G
import qualified Language.Haskell.TH.Syntax    as TH

import           Hasura.GraphQL.Utils
import           Hasura.SQL.Types
import           Hasura.SQL.Value

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
  } deriving (Show, Eq, TH.Lift)

fromEnumTyDef :: G.EnumTypeDefinition -> EnumTyInfo
fromEnumTyDef (G.EnumTypeDefinition descM n _ valDefs) =
  EnumTyInfo descM (G.NamedType n) $ Map.fromList
  [(G._evdName valDef, fromEnumValDef valDef) | valDef <- valDefs]

data InpValInfo
  = InpValInfo
  { _iviDesc :: !(Maybe G.Description)
  , _iviName :: !G.Name
  , _iviType :: !G.GType
  -- TODO, handle default values
  } deriving (Show, Eq, TH.Lift)

fromInpValDef :: G.InputValueDefinition -> InpValInfo
fromInpValDef (G.InputValueDefinition descM n ty _) =
  InpValInfo descM n ty

type ParamMap = Map.HashMap G.Name InpValInfo

data ObjFldInfo
  = ObjFldInfo
  { _fiDesc   :: !(Maybe G.Description)
  , _fiName   :: !G.Name
  , _fiParams :: !ParamMap
  , _fiTy     :: !G.GType
  } deriving (Show, Eq, TH.Lift)

fromFldDef :: G.FieldDefinition -> ObjFldInfo
fromFldDef (G.FieldDefinition descM n args ty _) =
  ObjFldInfo descM n params ty
  where
    params = Map.fromList [(G._ivdName arg, fromInpValDef arg) | arg <- args]

type ObjFieldMap = Map.HashMap G.Name ObjFldInfo

data ObjTyInfo
  = ObjTyInfo
  { _otiDesc   :: !(Maybe G.Description)
  , _otiName   :: !G.NamedType
  , _otiFields :: !ObjFieldMap
  } deriving (Show, Eq, TH.Lift)

mkObjTyInfo
  :: Maybe G.Description -> G.NamedType -> ObjFieldMap -> ObjTyInfo
mkObjTyInfo descM ty flds =
  ObjTyInfo descM ty $ Map.insert (_fiName typenameFld) typenameFld flds

typenameFld :: ObjFldInfo
typenameFld =
  ObjFldInfo (Just desc) "__typename" Map.empty $
  G.toGT $ G.toNT $ G.NamedType "String"
  where
    desc = "The name of the current Object type at runtime"

fromObjTyDef :: G.ObjectTypeDefinition -> ObjTyInfo
fromObjTyDef (G.ObjectTypeDefinition descM n _ _ flds) =
  mkObjTyInfo descM (G.NamedType n) $
  Map.fromList [(G._fldName fld, fromFldDef fld) | fld <- flds]

type InpObjFldMap = Map.HashMap G.Name InpValInfo

data InpObjTyInfo
  = InpObjTyInfo
  { _iotiDesc   :: !(Maybe G.Description)
  , _iotiName   :: !G.NamedType
  , _iotiFields :: !InpObjFldMap
  } deriving (Show, Eq, TH.Lift)

fromInpObjTyDef :: G.InputObjectTypeDefinition -> InpObjTyInfo
fromInpObjTyDef (G.InputObjectTypeDefinition descM n _ inpFlds) =
  InpObjTyInfo descM (G.NamedType n) $
  Map.fromList [(G._ivdName inpFld, fromInpValDef inpFld) | inpFld <- inpFlds]

data ScalarTyInfo
  = ScalarTyInfo
  { _stiDesc :: !(Maybe G.Description)
  , _stiType :: !PGColType
  } deriving (Show, Eq, TH.Lift)

fromScalarTyDef :: G.ScalarTypeDefinition -> Either Text ScalarTyInfo
fromScalarTyDef (G.ScalarTypeDefinition descM n _) =
  ScalarTyInfo descM <$> case n of
  "Int"     -> return PGInteger
  "Float"   -> return PGFloat
  "String"  -> return PGText
  "Boolean" -> return PGBoolean
  -- TODO: is this correct?
  "ID"      -> return PGText
  _         -> throwError $ "unexpected type: " <> G.unName n

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

fromTyDef :: G.TypeDefinition -> Either Text TypeInfo
fromTyDef = \case
  G.TypeDefinitionScalar t -> TIScalar <$> fromScalarTyDef t
  G.TypeDefinitionObject t -> return $ TIObj $ fromObjTyDef t
  G.TypeDefinitionInterface t ->
    throwError $ "unexpected interface: " <> showName (G._itdName t)
  G.TypeDefinitionUnion t ->
    throwError $ "unexpected union: " <> showName (G._utdName t)
  G.TypeDefinitionEnum t -> return $ TIEnum $ fromEnumTyDef t
  G.TypeDefinitionInputObject t -> return $ TIInpObj $ fromInpObjTyDef t

fromTyDefQ :: G.TypeDefinition -> TH.Q TH.Exp
fromTyDefQ tyDef = case fromTyDef tyDef of
  Left e  -> fail $ T.unpack e
  Right t -> TH.lift t

fromSchemaDocQ :: G.SchemaDocument -> TH.Q TH.Exp
fromSchemaDocQ (G.SchemaDocument tyDefs) =
  TH.ListE <$> mapM fromTyDefQ tyDefs

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
    args = Map.singleton "if" $ InpValInfo Nothing "if" $
           G.TypeNamed $ G.NamedType $ G.Name "Boolean"
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
  AGScalar _ Nothing   -> True
  AGEnum _ Nothing   -> True
  AGObject _ Nothing -> True
  AGArray _ Nothing  -> True
  _ -> False

getAnnInpValKind :: AnnGValue -> Text
getAnnInpValKind = \case
  AGScalar _ _ -> "scalar"
  AGEnum _ _   -> "enum"
  AGObject _ _ -> "object"
  AGArray _ _  -> "array"

getAnnInpValTy :: AnnGValue -> G.GType
getAnnInpValTy = \case
  AGScalar pct _ -> G.TypeNamed $ G.NamedType $ G.Name $ T.pack $ show pct
  AGEnum nt _    -> G.TypeNamed nt
  AGObject nt _  -> G.TypeNamed nt
  AGArray nt _   -> G.TypeList nt
