module Hasura.GraphQL.Schema.Common
  ( qualifiedObjectToName
  , textToName
  ) where

import           Hasura.Prelude

import           Language.GraphQL.Draft.Syntax (Name, mkName)

import           Hasura.RQL.Types
import           Hasura.SQL.Types

qualifiedObjectToName :: (ToTxt a, MonadError QErr m) => QualifiedObject a -> m Name
qualifiedObjectToName objectName = do
  let textName = snakeCaseQualObject objectName
  mkName textName `onNothing` throw400 ValidationFailed
    ("cannot include " <> objectName <<> " in the GraphQL schema because " <> textName
    <<> " is not a valid GraphQL identifier")

textToName :: MonadError QErr m => Text -> m Name
textToName textName = mkName textName `onNothing` throw400 ValidationFailed
                      ("cannot include " <> textName <<> " in the GraphQL schema because "
                       <> " it is not a valid GraphQL identifier")
