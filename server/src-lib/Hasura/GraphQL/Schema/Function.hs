module Hasura.GraphQL.Schema.Function
  ( procFuncArgs
  , mkFuncArgsInp
  , mkFuncQueryFld
  , mkFuncAggQueryFld
  ) where

import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Select
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

mkFuncArgsName :: QualifiedFunction -> G.Name
mkFuncArgsName fn =
  qualObjectToName fn <> "_args"

mkFuncArgsTy :: QualifiedFunction -> G.NamedType
mkFuncArgsTy =
  G.NamedType . mkFuncArgsName


{-
input function_args {
  arg1: arg-type1!
  .     .
  .     .
  argn: arg-typen!
}
-}

procFuncArgs
  :: Seq.Seq FunctionArg
  -> (PGColType -> Text -> a) -> [a]
procFuncArgs argSeq f =
  fst $ foldl mkItem ([], 1::Int) argSeq
  where
    mkItem (items, argNo) (FunctionArg nameM ty) =
      case nameM of
        Just argName ->
          let argT = getFuncArgNameTxt argName
          in (items <> pure (f ty argT), argNo)
        Nothing ->
          let argT = "arg_" <> T.pack (show argNo)
          in (items <> pure (f ty argT), argNo + 1)

mkFuncArgsInp :: FunctionInfo -> Maybe InpObjTyInfo
mkFuncArgsInp funcInfo =
  bool (Just inpObj) Nothing $ null funcArgs
  where
    funcName = fiName funcInfo
    funcArgs = fiInputArgs funcInfo
    funcArgsTy = mkFuncArgsTy funcName

    inpObj = mkHsraInpTyInfo Nothing funcArgsTy $
             fromInpValL argInps

    argInps = procFuncArgs funcArgs mkInpVal

    mkInpVal ty t =
      InpValInfo Nothing (G.Name t) Nothing $
      G.toGT $ mkScalarTy ty

{-

function(
  args: function_args
  where: table_bool_exp
  limit: Int
  offset: Int
): [table!]!

-}

mkFuncArgs :: FunctionInfo -> ParamMap
mkFuncArgs funInfo =
  fromInpValL $ funcInpArgs <> mkSelArgs retTable
  where
    funcName = fiName funInfo
    funcArgs = fiInputArgs funInfo
    retTable = fiReturnType funInfo

    funcArgDesc = G.Description $ "input parameters for function " <>> funcName
    funcInpArg = InpValInfo (Just funcArgDesc) "args" Nothing $ G.toGT $ G.toNT $
                 mkFuncArgsTy funcName
    funcInpArgs = bool [funcInpArg] [] $ null funcArgs

mkFuncQueryFld
  :: FunctionInfo -> ObjFldInfo
mkFuncQueryFld funInfo =
  mkHsraObjFldInfo (Just desc) fldName (mkFuncArgs funInfo) ty
  where
    retTable = fiReturnType funInfo
    funcName = fiName funInfo

    desc = G.Description $ "execute function " <> funcName
           <<> " which returns " <>> retTable
    fldName = qualObjectToName funcName

    ty      = G.toGT $ G.toNT $ G.toLT $ G.toNT $ mkTableTy retTable

{-

function_aggregate(
  args: function_args
  where: table_bool_exp
  limit: Int
  offset: Int
): table_aggregate!

-}

mkFuncAggQueryFld
  :: FunctionInfo -> ObjFldInfo
mkFuncAggQueryFld funInfo =
  mkHsraObjFldInfo (Just desc) fldName (mkFuncArgs funInfo) ty
  where
    funcName = fiName funInfo
    retTable = fiReturnType funInfo

    desc = G.Description $ "execute function " <> funcName
           <<> " and query aggregates on result of table type "
           <>> retTable

    fldName = qualObjectToName funcName <> "_aggregate"

    ty = G.toGT $ G.toNT $ mkTableAggTy retTable
