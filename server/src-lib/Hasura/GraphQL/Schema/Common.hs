module Hasura.GraphQL.Schema.Common
  ( qualObjectToName
  , addTypeSuffix
  , fromInpValL

  , RelationshipFieldInfo(..)
  , SelField

  , mkColumnType
  , mkRelName
  , mkAggRelName

  , mkTableTy
  , mkTableEnumType
  , mkTableAggTy

  , mkColumnEnumVal
  , mkDescriptionWith
  , mkDescription
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G

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

type SelField = Either PGColumnInfo RelationshipFieldInfo

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

mkDescriptionWith :: Maybe PGDescription -> Text -> G.Description
mkDescriptionWith descM defaultTxt = G.Description $ case descM of
  Nothing                      -> defaultTxt
  Just (PGDescription descTxt) -> T.unlines [descTxt, "\n", defaultTxt]

mkDescription :: PGDescription -> G.Description
mkDescription = G.Description . getPGDescription
