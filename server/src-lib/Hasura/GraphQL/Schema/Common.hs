module Hasura.GraphQL.Schema.Common
  ( qualObjectToName
  , fromInpValL

  , mkColName
  , mkRelName
  , mkAggRelName

  , SelField

  , mkTableTy
  , mkTableAggTy

  , mkColumnEnumVal
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

type SelField =
  Either PGColInfo (RelInfo, Bool, AnnBoolExpPartialSQL, Maybe Int, Bool)

qualObjectToName :: (ToTxt a) => QualifiedObject a -> G.Name
qualObjectToName = G.Name . snakeCaseQualObject

fromInpValL :: [InpValInfo] -> Map.HashMap G.Name InpValInfo
fromInpValL = mapFromL _iviName

mkRelName :: RelName -> G.Name
mkRelName rn = G.Name $ relNameToTxt rn

mkAggRelName :: RelName -> G.Name
mkAggRelName rn = G.Name $ relNameToTxt rn <> "_aggregate"

mkColName :: PGCol -> G.Name
mkColName (PGCol n) = G.Name n

mkTableTy :: QualifiedTable -> G.NamedType
mkTableTy =
  G.NamedType . qualObjectToName

mkTableAggTy :: QualifiedTable -> G.NamedType
mkTableAggTy tn =
  G.NamedType $ qualObjectToName tn <> "_aggregate"

-- used for 'distinct_on' in select and upsert's 'update columns'
mkColumnEnumVal :: PGCol -> EnumValInfo
mkColumnEnumVal (PGCol col) =
  EnumValInfo (Just "column name") (G.EnumValue $ G.Name col) False
