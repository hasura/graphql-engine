module Hasura.GraphQL.Schema.Common
  ( ColField(..)
  , onlyIntCols
  , onlyNumCols
  , onlyJSONBCols
  , onlyComparableCols

  , qualObjectToName
  , addTypeSuffix
  , fromInpValL

  , mkColumnType
  , mkRelName
  , mkAggRelName

  , SelField

  , mkTableTy
  , mkTableEnumType
  , mkTableAggTy

  , mkColumnEnumVal
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

data ColField
  = ColField
  { _cfPGCol :: !PGColumnInfo
  , _cfName  :: !G.Name
  } deriving (Show, Eq)

onlyIntCols :: [ColField] -> [ColField]
onlyIntCols = filter (isScalarColumnWhere isIntegerType . pgiType . _cfPGCol)

onlyNumCols :: [ColField] -> [ColField]
onlyNumCols = filter (isScalarColumnWhere isNumType . pgiType . _cfPGCol)

onlyJSONBCols :: [ColField] -> [ColField]
onlyJSONBCols = filter (isScalarColumnWhere (== PGJSONB) . pgiType . _cfPGCol)

onlyComparableCols :: [ColField] -> [ColField]
onlyComparableCols = filter (isScalarColumnWhere isComparableType . pgiType . _cfPGCol)

type SelField = Either ColField
                ( RelInfo
                , Bool
                , PGColGNameMap
                , AnnBoolExpPartialSQL
                , Maybe Int
                , Bool
                )

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
