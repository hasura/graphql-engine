module Hasura.GraphQL.Schema.Mutation.Delete
  ( mkDelMutFld
  ) where

import qualified Language.GraphQL.Draft.Syntax         as G

import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Mutation.Common
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.SQL.Types

{-

delete_table(
  where : table_bool_exp!
): table_mutation_response

-}

mkDelMutFld
  :: QualifiedTable -> ObjFldInfo
mkDelMutFld tn =
  mkHsraObjFldInfo (Just desc) fldName (fromInpValL [filterArg]) $
    G.toGT $ mkMutRespTy tn
  where
    desc = G.Description $ "delete data from the table: " <>> tn

    fldName = "delete_" <> qualObjectToName tn

    filterArgDesc = "filter the rows which have to be deleted"
    filterArg =
      InpValInfo (Just filterArgDesc) "where" Nothing $ G.toGT $
      G.toNT $ mkBoolExpTy tn
