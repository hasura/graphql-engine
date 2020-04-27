module Hasura.GraphQL.Schema.Common
  ( textToName
  , partialSQLExpToUnpreparedValue
  ) where

import           Hasura.Prelude

import           Language.GraphQL.Draft.Syntax (Name, mkName)

import qualified Hasura.GraphQL.Parser         as P

import           Hasura.RQL.Types
import           Hasura.SQL.Types

textToName :: MonadError QErr m => Text -> m Name
textToName textName = mkName textName `onNothing` throw400 ValidationFailed
                      ("cannot include " <> textName <<> " in the GraphQL schema because "
                       <> " it is not a valid GraphQL identifier")

partialSQLExpToUnpreparedValue :: PartialSQLExp -> P.UnpreparedValue
partialSQLExpToUnpreparedValue (PSESessVar pftype var) = P.UVSessionVar pftype var
partialSQLExpToUnpreparedValue (PSESQLExp sqlExp)      = P.UVLiteral sqlExp
