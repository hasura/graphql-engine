module Hasura.GraphQL.Schema.Common
  ( ColField(..)
  , onlyIntCols
  , onlyNumCols
  , onlyJSONBCols
  , onlyComparableCols

  , qualObjectToName
  , fromInpValL

  , mkRelName
  , mkAggRelName

  , SelField

  , mkTableTy
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
  { _cfPGCol :: !PGColInfo
  , _cfName  :: !G.Name
  } deriving (Show, Eq)

onlyIntCols :: [ColField] -> [ColField]
onlyIntCols = filter (isIntegerType . pgiType . _cfPGCol)

onlyNumCols :: [ColField] -> [ColField]
onlyNumCols = filter (isNumType . pgiType . _cfPGCol)

onlyJSONBCols :: [ColField] -> [ColField]
onlyJSONBCols = filter (isJSONBType . pgiType . _cfPGCol)

onlyComparableCols :: [ColField] -> [ColField]
onlyComparableCols = filter (isComparableType . pgiType . _cfPGCol)

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

fromInpValL :: [InpValInfo] -> Map.HashMap G.Name InpValInfo
fromInpValL = mapFromL _iviName

mkRelName :: RelName -> G.Name
mkRelName rn = G.Name $ relNameToTxt rn

mkAggRelName :: RelName -> G.Name
mkAggRelName rn = G.Name $ relNameToTxt rn <> "_aggregate"

mkTableTy :: QualifiedTable -> G.NamedType
mkTableTy =
  G.NamedType . qualObjectToName

mkTableAggTy :: QualifiedTable -> G.NamedType
mkTableAggTy tn =
  G.NamedType $ qualObjectToName tn <> "_aggregate"

-- used for 'distinct_on' in select and upsert's 'update columns'
mkColumnEnumVal :: G.Name -> EnumValInfo
mkColumnEnumVal colName =
  EnumValInfo (Just "column name") (G.EnumValue colName) False
