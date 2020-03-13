module Hasura.GraphQL.Schema.Merge
  ( checkSchemaConflicts
  , checkConflictingNode
  ) where


import           Data.Maybe                    (maybeToList)

import qualified Data.HashMap.Strict           as Map
import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types

checkSchemaConflicts
  :: (MonadError QErr m)
  => GCtx -> GCtx -> m ()
checkSchemaConflicts gCtx remoteCtx = do
  let typeMap     = _gTypes gCtx -- hasura typemap
  -- check type conflicts
  let hTypes      = Map.elems typeMap
      hTyNames    = map G.unNamedType $ Map.keys typeMap
      -- get the root names from the remote schema
      rmQRootName = _otiName $ _gQueryRoot remoteCtx
      rmMRootName = maybeToList $ _otiName <$> _gMutRoot remoteCtx
      rmSRootName = maybeToList $ _otiName <$> _gSubRoot remoteCtx
      rmRootNames = map G.unNamedType (rmQRootName:(rmMRootName ++ rmSRootName))
  let rmTypes     = Map.filterWithKey
                    (\k _ -> G.unNamedType k `notElem` builtinTy ++ rmRootNames)
                    $ _gTypes remoteCtx

      isTyInfoSame ty = any (`tyinfoEq` ty) hTypes
      -- name is same and structure is not same
      isSame n ty = G.unNamedType n `elem` hTyNames &&
                    not (isTyInfoSame ty)
      conflictedTypes = Map.filterWithKey isSame rmTypes
      conflictedTyNames = map G.unNamedType $ Map.keys conflictedTypes

  unless (Map.null conflictedTypes) $
    throw400 RemoteSchemaConflicts $ tyMsg conflictedTyNames

  -- check node conflicts
  let rmQRoot = _otiFields $ _gQueryRoot remoteCtx
      rmMRoot = _otiFields <$> _gMutRoot remoteCtx
      rmRoots = filter (`notElem` builtinNodes ++ rmRootNames) . Map.keys <$>
                mergeMaybeMaps (Just rmQRoot) rmMRoot
      hQR     = _otiFields <$>
                (getObjTyM =<< Map.lookup hQRName typeMap)
      hMR     = _otiFields <$>
                (getObjTyM =<< Map.lookup hMRName typeMap)
      hRoots  = Map.keys <$> mergeMaybeMaps hQR hMR

  case (rmRoots, hRoots) of
    (Just rmR, Just hR) -> do
      let conflictedNodes = filter (`elem` hR) rmR
      unless (null conflictedNodes) $
        throw400 RemoteSchemaConflicts $ nodesMsg conflictedNodes
    _ -> return ()

  where
    tyinfoEq a b = case (a, b) of
      (TIScalar t1, TIScalar t2) -> typeEq t1 t2
      (TIObj t1, TIObj t2)       -> typeEq t1 t2
      (TIEnum t1, TIEnum t2)     -> typeEq t1 t2
      (TIInpObj t1, TIInpObj t2) -> typeEq t1 t2
      _                          -> False

    hQRName = G.NamedType "query_root"
    hMRName = G.NamedType "mutation_root"
    tyMsg ty = "types: [ " <> namesToTxt ty <>
               " ] have mismatch with current graphql schema. HINT: Types must be same."
    nodesMsg n = "top-level nodes: [ " <> namesToTxt n <>
                 " ] already exist in current graphql schema. HINT: Top-level nodes can't be same."
    namesToTxt = T.intercalate ", " . map G.unName
    builtinNodes = ["__type", "__schema", "__typename"]
    builtinTy = [ "__Directive"
                , "__DirectiveLocation"
                , "__EnumValue"
                , "__Field"
                , "__InputValue"
                , "__Schema"
                , "__Type"
                , "__TypeKind"
                , "Int"
                , "Float"
                , "String"
                , "Boolean"
                , "ID"
                ]

checkConflictingNode
  :: (MonadError QErr m)
  => GCtx
  -> G.Name
  -> m ()
checkConflictingNode gCtx node = do
  let typeMap = _gTypes gCtx
      hQR     = _otiFields <$>
                (getObjTyM =<< Map.lookup hQRName typeMap)
      hMR     = _otiFields <$>
                (getObjTyM =<< Map.lookup hMRName typeMap)
      hRoots  = Map.keys <$> mergeMaybeMaps hQR hMR
  case hRoots of
    Just hR ->
      when (node `elem` hR) $
        throw400 RemoteSchemaConflicts msg
    _ -> return ()
  where
    hQRName = G.NamedType "query_root"
    hMRName = G.NamedType "mutation_root"
    msg = "node " <> G.unName node <>
          " already exists in current graphql schema"

mergeMaybeMaps
  :: (Eq k, Hashable k)
  => Maybe (Map.HashMap k v)
  -> Maybe (Map.HashMap k v)
  -> Maybe (Map.HashMap k v)
mergeMaybeMaps m1 m2 = case (m1, m2) of
  (Nothing, Nothing)   -> Nothing
  (Just m1', Nothing)  -> Just m1'
  (Nothing, Just m2')  -> Just m2'
  (Just m1', Just m2') -> Just $ Map.union m1' m2'
