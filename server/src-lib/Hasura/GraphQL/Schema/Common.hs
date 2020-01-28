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

  , mkColumnType
  , mkRelName
  , mkAggRelName
  , mkComputedFieldName

  , mkTableTy
  , mkTableEnumType
  , mkTableAggTy

  , mkColumnEnumVal
  , mkDescriptionWith
  , mkDescription

  , mkFuncArgsTy

  , AggregateOp(..)
  , AggregateOpName(..)
  , aggregateOpReturnTypeToGType
  , allAggregateOps
  , aggregateOpRequiredScalars
  , aggregateOpToName
  , isAggField
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G

import           Control.Lens
import           Control.Lens.TH               (makePrisms)

import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

data RelationshipFieldInfo
  = RelationshipFieldInfo
  { _rfiInfo       :: !RelInfo
  , _rfiAllowAgg   :: !Bool
  , _rfiColumns    :: !PGColGNameMap
  , _rfiPermFilter :: !AnnBoolExpPartialSQL
  , _rfiPermLimit  :: !(Maybe Int)
  , _rfiIsNullable :: !Bool
  } deriving (Show, Eq)

data SelField
  = SFPGColumn !PGColumnInfo
  | SFRelationship !RelationshipFieldInfo
  | SFComputedField !ComputedField
  deriving (Show, Eq)
$(makePrisms ''SelField)

getPGColumnFields :: [SelField] -> [PGColumnInfo]
getPGColumnFields = mapMaybe (^? _SFPGColumn)

getRelationshipFields :: [SelField] -> [RelationshipFieldInfo]
getRelationshipFields = mapMaybe (^? _SFRelationship)

getComputedFields :: [SelField] -> [ComputedField]
getComputedFields = mapMaybe (^? _SFComputedField)

qualObjectToName :: (ToTxt a) => QualifiedObject a -> G.Name
qualObjectToName = G.Name . snakeCaseQualObject

addTypeSuffix :: Text -> G.NamedType -> G.NamedType
addTypeSuffix suffix baseType = G.NamedType $ G.unNamedType baseType <> G.Name suffix

fromInpValL :: [InpValInfo] -> Map.HashMap G.Name InpValInfo
fromInpValL = mapFromL _iviName

mkRelName :: RelName -> G.Name
mkRelName rn = G.Name $ relNameToTxt rn

mkAggRelName :: RelName -> G.Name
mkAggRelName rn = G.Name $ relNameToTxt rn <> "_aggregate"

mkComputedFieldName :: ComputedFieldName -> G.Name
mkComputedFieldName = G.Name . computedFieldNameToText

mkColumnType :: PGColumnType -> G.NamedType
mkColumnType = \case
  PGColumnScalar scalarType -> mkScalarTy scalarType
  PGColumnEnumReference (EnumReference enumTable _) -> mkTableEnumType enumTable

mkTableTy :: QualifiedTable -> G.NamedType
mkTableTy = G.NamedType . qualObjectToName

mkTableEnumType :: QualifiedTable -> G.NamedType
mkTableEnumType = addTypeSuffix "_enum" . mkTableTy

mkTableAggTy :: QualifiedTable -> G.NamedType
mkTableAggTy = addTypeSuffix "_aggregate" . mkTableTy

-- used for 'distinct_on' in select and upsert's 'update columns'
mkColumnEnumVal :: G.Name -> EnumValInfo
mkColumnEnumVal colName =
  EnumValInfo (Just "column name") (G.EnumValue colName) False

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

newtype AggregateOpName = AggregateOpName {unAggregateOpName :: Text}
  deriving (Show, Eq, IsString)

data AggregateOpReturnType
  = AORTFromArgumentType !(PGColumnType -> G.GType)
  | AORTStatic !PGScalarType

aggregateOpReturnTypeToGType :: AggregateOpReturnType -> PGColumnType -> G.GType
aggregateOpReturnTypeToGType = \case
  AORTFromArgumentType fn -> fn
  AORTStatic ty           -> const $ G.toGT $ mkScalarTy ty

data AggregateOp
  = AggregateOp
    { _aoOpName             :: !AggregateOpName
    , _aoArgumentTypeFilter :: !(PGScalarType -> Bool)
    , _aoReturnType         :: !AggregateOpReturnType
    }

aggregateOpToName :: AggregateOp -> G.Name
aggregateOpToName = G.Name . unAggregateOpName . _aoOpName

allAggregateOps :: [AggregateOp]
allAggregateOps = numericAggOps <> compareAggOps <> boolOps <> [arrayAggOp]
  where
    sameAsArgType = AORTFromArgumentType (G.toGT . mkColumnType)
    numericAggOps = sumOp:otherOps
      where
        sumOp = AggregateOp "sum" isNumType sameAsArgType
        alwaysFloat = AORTStatic PGFloat
        otherOps = flip map otherOpNames $ \name ->
                     AggregateOp name isNumType alwaysFloat
        otherOpNames = [ "avg", "stddev", "stddev_samp", "stddev_pop"
                       , "variance", "var_samp", "var_pop"
                       ]

    compareAggOps = flip map ["max", "min"] $ \name ->
                    AggregateOp name isComparableType sameAsArgType

    arrayAggOp = AggregateOp "array_agg" (const True) $
                 AORTFromArgumentType (G.toGT . G.toLT . G.toGT . mkColumnType)

    boolOps = flip map ["bool_and", "bool_or"] $ \name ->
              AggregateOp name (== PGBoolean) $ AORTStatic PGBoolean

aggregateOpRequiredScalars :: Set.HashSet PGScalarType
aggregateOpRequiredScalars = Set.fromList $ flip concatMap allAggregateOps $
  \aggOp -> case _aoReturnType aggOp of
           AORTFromArgumentType _ -> []
           AORTStatic scalarTy    -> [scalarTy]

isAggField :: G.Name -> Bool
isAggField =
  flip elem (map _aoOpName allAggregateOps) . AggregateOpName . G.unName
