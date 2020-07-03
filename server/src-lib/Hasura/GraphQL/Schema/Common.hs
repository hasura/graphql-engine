module Hasura.GraphQL.Schema.Common
  ( qualObjectToName
  , addTypeSuffix
  , fromInpValL

  , RelationshipFieldInfo(..)
  , SelField(..)
  , _SFPGColumn
  , getPGColumnFields
  , getRelationshipFields
  , getComputedFields
  , getRemoteRelationships

  , mkColumnType
  , mkRelName
  , mkAggRelName
  , mkConnectionRelName
  , mkComputedFieldName

  , mkTableTy
  , mkTableConnectionTy
  , mkTableEdgeTy
  , mkTableEnumType
  , mkTableAggTy

  , mkColumnEnumVal
  , mkColumnInputVal
  , mkDescriptionWith

  , mkFuncArgsTy

  , mkPGColGNameMap

  , numAggregateOps
  , compAggregateOps

  , nodeType
  , nodeIdType
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G

import           Control.Lens

import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

data RelationshipFieldInfo
  = RelationshipFieldInfo
  { _rfiInfo              :: !RelInfo
  , _rfiAllowAgg          :: !Bool
  , _rfiColumns           :: !PGColGNameMap
  , _rfiPermFilter        :: !AnnBoolExpPartialSQL
  , _rfiPermLimit         :: !(Maybe Int)
  , _rfiPrimaryKeyColumns :: !(Maybe PrimaryKeyColumns)
  , _rfiIsNullable        :: !Bool
  } deriving (Show, Eq)

data SelField
  = SFPGColumn !PGColumnInfo
  | SFRelationship !RelationshipFieldInfo
  | SFComputedField !ComputedField
  | SFRemoteRelationship !RemoteFieldInfo
  deriving (Show, Eq)
$(makePrisms ''SelField)

getPGColumnFields :: [SelField] -> [PGColumnInfo]
getPGColumnFields = mapMaybe (^? _SFPGColumn)

getRelationshipFields :: [SelField] -> [RelationshipFieldInfo]
getRelationshipFields = mapMaybe (^? _SFRelationship)

getComputedFields :: [SelField] -> [ComputedField]
getComputedFields = mapMaybe (^? _SFComputedField)

getRemoteRelationships :: [SelField] -> [RemoteFieldInfo]
getRemoteRelationships = mapMaybe (^? _SFRemoteRelationship)

qualObjectToName :: (ToTxt a) => QualifiedObject a -> G.Name
qualObjectToName = G.Name . snakeCaseQualObject

addTypeSuffix :: Text -> G.NamedType -> G.NamedType
addTypeSuffix suffix baseType =
  G.NamedType $ G.unNamedType baseType <> G.Name suffix

fromInpValL :: [InpValInfo] -> Map.HashMap G.Name InpValInfo
fromInpValL = mapFromL _iviName

mkRelName :: RelName -> G.Name
mkRelName rn = G.Name $ relNameToTxt rn

mkAggRelName :: RelName -> G.Name
mkAggRelName rn = G.Name $ relNameToTxt rn <> "_aggregate"

mkConnectionRelName :: RelName -> G.Name
mkConnectionRelName rn = G.Name $ relNameToTxt rn <> "_connection"

mkComputedFieldName :: ComputedFieldName -> G.Name
mkComputedFieldName = G.Name . computedFieldNameToText

mkColumnType :: PGColumnType -> G.NamedType
mkColumnType = \case
  PGColumnScalar scalarType -> mkScalarTy scalarType
  PGColumnEnumReference (EnumReference enumTable _) -> mkTableEnumType enumTable

mkTableTy :: QualifiedTable -> G.NamedType
mkTableTy = G.NamedType . qualObjectToName

mkTableConnectionTy :: QualifiedTable -> G.NamedType
mkTableConnectionTy = addTypeSuffix "Connection" . mkTableTy

mkTableEdgeTy :: QualifiedTable -> G.NamedType
mkTableEdgeTy = addTypeSuffix "Edge" . mkTableTy

mkTableEnumType :: QualifiedTable -> G.NamedType
mkTableEnumType = addTypeSuffix "_enum" . mkTableTy

mkTableAggTy :: QualifiedTable -> G.NamedType
mkTableAggTy = addTypeSuffix "_aggregate" . mkTableTy

-- used for 'distinct_on' in select and upsert's 'update columns'
mkColumnEnumVal :: G.Name -> EnumValInfo
mkColumnEnumVal colName =
  EnumValInfo (Just "column name") (G.EnumValue colName) False

mkColumnInputVal :: PGColumnInfo -> InpValInfo
mkColumnInputVal ci =
  InpValInfo (mkDescription <$> pgiDescription ci) (pgiName ci)
  Nothing $ G.toGT $ G.toNT $ mkColumnType $ pgiType ci

mkDescriptionWith :: Maybe PGDescription -> Text -> G.Description
mkDescriptionWith descM defaultTxt = G.Description $ case descM of
  Nothing                      -> defaultTxt
  Just (PGDescription descTxt) -> T.unlines [descTxt, "\n", defaultTxt]

mkDescription :: PGDescription -> G.Description
mkDescription = G.Description . getPGDescription

mkFuncArgsName :: QualifiedFunction -> G.Name
mkFuncArgsName fn =
  qualObjectToName fn <> "_args"

mkFuncArgsTy :: QualifiedFunction -> G.NamedType
mkFuncArgsTy =
  G.NamedType . mkFuncArgsName

mkPGColGNameMap :: [PGColumnInfo] -> PGColGNameMap
mkPGColGNameMap cols = Map.fromList $
  flip map cols $ \ci -> (pgiName ci, ci)

numAggregateOps :: [G.Name]
numAggregateOps = [ "sum", "avg", "stddev", "stddev_samp", "stddev_pop"
            , "variance", "var_samp", "var_pop"
            ]

compAggregateOps :: [G.Name]
compAggregateOps = ["max", "min"]

nodeType :: G.NamedType
nodeType =
  G.NamedType "Node"

nodeIdType :: G.GType
nodeIdType =
  G.toGT $ G.toNT $ G.NamedType "ID"
