module Hasura.GraphQL.Schema.Common
  ( qualObjectToName
  , addTypeSuffix
  , fromInpValL

  , mkColName
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

import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

type SelField = Either PGColumnInfo (RelInfo, Bool, AnnBoolExpPartialSQL, Maybe Int, Bool)

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

mkColName :: PGCol -> G.Name
mkColName (PGCol n) = G.Name n

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
mkColumnEnumVal :: PGCol -> EnumValInfo
mkColumnEnumVal (PGCol col) =
  EnumValInfo (Just "column name") (G.EnumValue $ G.Name col) False
