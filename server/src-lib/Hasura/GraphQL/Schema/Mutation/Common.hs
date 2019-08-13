module Hasura.GraphQL.Schema.Mutation.Common
  ( mkPGColInp
  , mkMutRespTy
  , mkMutRespObj
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

mkPGColInp :: PGColInfo -> InpValInfo
mkPGColInp (PGColInfo colName colTy _) =
  InpValInfo Nothing (G.Name $ getPGColTxt colName) Nothing $
  G.toGT $ mkScalarTy colTy

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
