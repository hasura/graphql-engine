module Hasura.GraphQL.Schema.Function
  ( procFuncArgs
  , mkFuncArgsInp
  , mkFuncQueryFld
  , mkFuncQueryConnectionFld
  , mkFuncAggQueryFld
  , mkFuncArgsTy
  , mkFuncArgItemSeq
  ) where

import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Select
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

{-
input function_args {
  arg1: arg-type1!
  .     .
  .     .
  argn: arg-typen!
}
-}

procFuncArgs :: Seq.Seq a -> (a -> Maybe FunctionArgName) -> (a -> Text -> b) -> [b]
procFuncArgs argSeq nameFn resultFn =
  fst $ foldl mkItem ([], 1::Int) argSeq
  where
    mkItem (items, argNo) fa =
      case nameFn fa of
        Just argName ->
          let argT = getFuncArgNameTxt argName
          in (items <> pure (resultFn fa argT), argNo)
        Nothing ->
          let argT = "arg_" <> T.pack (show argNo)
          in (items <> pure (resultFn fa argT), argNo + 1)

mkFuncArgsInp :: QualifiedFunction -> Seq.Seq FunctionArg -> Maybe InpObjTyInfo
mkFuncArgsInp funcName funcArgs =
  bool (Just inpObj) Nothing $ null funcArgs
  where
    funcArgsTy = mkFuncArgsTy funcName

    inpObj = mkHsraInpTyInfo Nothing funcArgsTy $
             fromInpValL argInps

    argInps = procFuncArgs funcArgs faName mkInpVal

    mkInpVal fa t =
      InpValInfo Nothing (G.Name t) Nothing $
      G.toGT $ mkScalarTy $ _qptName $ faType fa

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
    funcArgs = getInputArgs funInfo
    retTable = fiReturnType funInfo

    funcArgDesc = G.Description $ "input parameters for function " <>> funcName
    funcInpArg = InpValInfo (Just funcArgDesc) "args" Nothing $ G.toGT $ G.toNT $
                 mkFuncArgsTy funcName
    funcInpArgs = bool [funcInpArg] [] $ null funcArgs

mkFuncQueryFld
  :: FunctionInfo -> Maybe PGDescription -> ObjFldInfo
mkFuncQueryFld funInfo descM =
  mkHsraObjFldInfo (Just desc) fldName (mkFuncArgs funInfo) ty
  where
    retTable = fiReturnType funInfo
    funcName = fiName funInfo

    desc = mkDescriptionWith descM $ "execute function " <> funcName
           <<> " which returns " <>> retTable
    fldName = qualObjectToName funcName

    ty      = G.toGT $ G.toNT $ G.toLT $ G.toNT $ mkTableTy retTable

mkFuncQueryConnectionFld
  :: FunctionInfo -> Maybe PGDescription -> ObjFldInfo
mkFuncQueryConnectionFld funInfo descM =
  mkHsraObjFldInfo (Just desc) fldName (mkFuncArgs funInfo) ty
  where
    retTable = fiReturnType funInfo
    funcName = fiName funInfo

    desc = mkDescriptionWith descM $ "execute function " <> funcName
           <<> " which returns " <>> retTable
    fldName = qualObjectToName funcName <> "_connection"

    ty = G.toGT $ G.toNT $ mkTableConnectionTy retTable

{-

function_aggregate(
  args: function_args
  where: table_bool_exp
  limit: Int
  offset: Int
): table_aggregate!

-}

mkFuncAggQueryFld
  :: FunctionInfo -> Maybe PGDescription -> ObjFldInfo
mkFuncAggQueryFld funInfo descM =
  mkHsraObjFldInfo (Just desc) fldName (mkFuncArgs funInfo) ty
  where
    funcName = fiName funInfo
    retTable = fiReturnType funInfo

    desc = mkDescriptionWith descM $ "execute function " <> funcName
           <<> " and query aggregates on result of table type "
           <>> retTable

    fldName = qualObjectToName funcName <> "_aggregate"

    ty = G.toGT $ G.toNT $ mkTableAggTy retTable


mkFuncArgItemSeq :: FunctionInfo -> Seq (InputArgument FunctionArgItem)
mkFuncArgItemSeq functionInfo =
  let inputArgs = fiInputArgs functionInfo
  in Seq.fromList $ procFuncArgs inputArgs nameFn resultFn
  where
    nameFn = \case
      IAUserProvided fa       -> faName fa
      IASessionVariables name -> Just name
    resultFn arg gName = flip fmap arg $
      \fa -> FunctionArgItem (G.Name gName) (faName fa) (faHasDefault fa)
