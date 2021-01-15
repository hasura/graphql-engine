module Hasura.RQL.DDL.Deps
       ( reportDeps
       , reportDepsExt
       )
       where

import           Hasura.Prelude

import           Data.Text.Extended

import           Hasura.RQL.Types

reportDeps :: (QErrM m) => [SchemaObjId] -> m ()
reportDeps deps =
  throw400 DependencyError $
    "cannot drop due to the following dependent objects : "
     <> reportSchemaObjs deps

reportDepsExt :: (QErrM m) => [SchemaObjId] -> [Text] -> m ()
reportDepsExt deps unknownDeps =
  throw400 DependencyError $
    "cannot drop due to the following dependent objects : " <> depObjsTxt
  where
    depObjsTxt = commaSeparated $ reportSchemaObjs deps:unknownDeps
