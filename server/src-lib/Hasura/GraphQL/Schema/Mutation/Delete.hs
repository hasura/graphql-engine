module Hasura.GraphQL.Schema.Mutation.Delete
  ( mkDelMutFld
  , mkDeleteByPkMutationField
  ) where

import qualified Language.GraphQL.Draft.Syntax         as G

import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Mutation.Common
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

{-

delete_table(
  where : table_bool_exp!
): table_mutation_response

-}

mkDelMutFld :: Maybe G.Name -> QualifiedTable -> ObjFldInfo
mkDelMutFld mCustomName tn =
  mkHsraObjFldInfo (Just desc) fldName (fromInpValL [filterArg]) $
    G.toGT $ mkMutRespTy tn
  where
    desc = G.Description $ "delete data from the table: " <>> tn

    defFldName = "delete_" <> qualObjectToName tn
    fldName = fromMaybe defFldName mCustomName

    filterArgDesc = "filter the rows which have to be deleted"
    filterArg =
      InpValInfo (Just filterArgDesc) "where" Nothing $ G.toGT $
      G.toNT $ mkBoolExpTy tn

{-
delete_table_by_pk(
  col1: col-ty1!
  col2: col-ty2!
): table
-}

mkDeleteByPkMutationField
  :: Maybe G.Name
  -> QualifiedTable
  -> PrimaryKey PGColumnInfo
  -> ObjFldInfo
mkDeleteByPkMutationField mCustomName qt primaryKey =
  mkHsraObjFldInfo (Just description) fieldName (fromInpValL inputArgs) $
  G.toGT $ mkTableTy qt
  where
    description = G.Description $ "delete single row from the table: " <>> qt
    fieldName = flip fromMaybe mCustomName $ "delete_" <> qualObjectToName qt <> "_by_pk"
    inputArgs = map mkColumnInputVal $ toList $ _pkColumns primaryKey
