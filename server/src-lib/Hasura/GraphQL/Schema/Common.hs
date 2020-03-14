module Hasura.GraphQL.Schema.Common
  ( qualifiedObjectToName
  ) where

import Hasura.Prelude

import Language.GraphQL.Draft.Syntax (Name, mkName)

import Hasura.SQL.Types
import Hasura.RQL.Types

qualifiedObjectToName :: (ToTxt a, MonadError QErr m) => QualifiedObject a -> m Name
qualifiedObjectToName objectName = do
  let textName = snakeCaseQualObject objectName
  mkName textName `onNothing` throw400 ValidationFailed
    ("cannot include " <> objectName <<> " in the GraphQL schema because " <> textName
    <<> " is not a valid GraphQL identifier")
