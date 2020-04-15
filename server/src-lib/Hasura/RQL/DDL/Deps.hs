module Hasura.RQL.DDL.Deps
       ( reportDeps
       , reportDepsExt
       )
       where

import           Hasura.Prelude

import qualified Data.Text         as T

import           Hasura.RQL.Types

reportDeps :: (QErrM m) => [SchemaObjId] -> m ()
reportDeps deps =
  throw400 DependencyError $
    "cannot drop due to the following dependent objects : "
     <> reportSchemaObjs deps

reportDepsExt :: (QErrM m) => [SchemaObjId] -> [T.Text] -> m ()
reportDepsExt deps unknownDeps =
  throw400 DependencyError $
    "cannot drop due to the following dependent objects : " <> depObjsTxt
  where
    depObjsTxt = T.intercalate ", " (reportSchemaObjs deps:unknownDeps)
