module Hasura.GraphQL.Schema.Mutation.Common
  ( mkPGColInp
  , mkMutRespTy
  , mkMutRespObj
  , mkPKeyColumnsInpObj
  , primaryKeyColumnsInp
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

mkPGColInp :: PGColumnInfo -> InpValInfo
mkPGColInp ci =
  InpValInfo Nothing (pgiName ci) Nothing $ G.toGT $ mkColumnType $ pgiType ci

-- table_mutation_response
mkMutRespTy :: QualifiedTable -> G.NamedType
mkMutRespTy tn =
  G.NamedType $ qualObjectToName tn <> "_mutation_response"

{-
type table_mutation_response {
  affected_rows: Int!
  returning: [table!]!
}
-}
mkMutRespObj
  :: QualifiedTable
  -> Bool -- is sel perm defined
  -> ObjTyInfo
mkMutRespObj tn sel =
  mkHsraObjTyInfo (Just objDesc) (mkMutRespTy tn) Set.empty $ mapFromL _fiName
    $ affectedRowsFld : bool [] [returningFld] sel
  where
    objDesc = G.Description $
      "response of any mutation on the table " <>> tn
    affectedRowsFld =
      mkHsraObjFldInfo (Just desc) "affected_rows" Map.empty $
        G.toGT $ G.toNT $ mkScalarTy PGInteger
      where
        desc = "number of affected rows by the mutation"
    returningFld =
      mkHsraObjFldInfo (Just desc) "returning" Map.empty $
        G.toGT $ G.toNT $ G.toLT $ G.toNT $ mkTableTy tn
      where
        desc = "data of the affected rows by the mutation"

{-
input table_pk_columns_input {
  col1: col-ty1!
  col2: col-ty2!
}

where col1, col2 are primary key columns
-}

mkPKeyColumnsInpTy :: QualifiedTable -> G.NamedType
mkPKeyColumnsInpTy qt =
  G.NamedType $ qualObjectToName qt <> "_pk_columns_input"

mkPKeyColumnsInpObj :: QualifiedTable -> PrimaryKey PGColumnInfo -> InpObjTyInfo
mkPKeyColumnsInpObj qt primaryKey =
  mkHsraInpTyInfo (Just description) (mkPKeyColumnsInpTy qt) $
  fromInpValL $ map mkColumnInputVal $ toList $ _pkColumns primaryKey
  where
    description = G.Description $ "primary key columns input for table: " <>> qt

primaryKeyColumnsInp :: QualifiedTable -> InpValInfo
primaryKeyColumnsInp qt =
  InpValInfo Nothing "pk_columns" Nothing $ G.toGT $ G.toNT $
  mkPKeyColumnsInpTy qt
