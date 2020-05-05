module Hasura.GraphQL.Schema.Common where

import           Hasura.Prelude

import           Language.GraphQL.Draft.Syntax         as G

import qualified Hasura.GraphQL.Parser                 as P

import           Hasura.GraphQL.Parser                 (FieldsParser, Kind (..))
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Internal.Parser (ifDefinitions)
import           Hasura.RQL.Types
import           Hasura.SQL.Types

textToName :: MonadError QErr m => Text -> m G.Name
textToName textName = G.mkName textName `onNothing` throw400 ValidationFailed
                      ("cannot include " <> textName <<> " in the GraphQL schema because "
                       <> " it is not a valid GraphQL identifier")

partialSQLExpToUnpreparedValue :: PartialSQLExp -> P.UnpreparedValue
partialSQLExpToUnpreparedValue (PSESessVar pftype var) = P.UVSessionVar pftype var
partialSQLExpToUnpreparedValue (PSESQLExp sqlExp)      = P.UVLiteral sqlExp

mapField
  :: Functor m
  => P.FieldsParser k m (Maybe a)
  -> (a -> b)
  -> P.FieldsParser k m (Maybe b)
mapField fp f = fmap (fmap f) fp

numericAggOperators :: [G.Name]
numericAggOperators =
  [ $$(G.litName "sum")
  , $$(G.litName "avg")
  , $$(G.litName "stddev")
  , $$(G.litName "stddev_samp")
  , $$(G.litName "stddev_pop")
  , $$(G.litName "variance")
  , $$(G.litName "var_samp")
  , $$(G.litName "var_pop")
  ]

comparisonAggOperators :: [G.Name]
comparisonAggOperators = [$$(litName "max"), $$(litName "min")]


-- | Typename field
--
-- WIP NOTE: this is a bit of a hack, and needs to be changed.
--
-- Every selection set is required by the standard to accept a special
-- field named "__typename". However, it should *not* be exposed as
-- part of the schema. A better way to implement this would be in
-- `selectionSet` directly, but it poses its own set of challenges, as
-- for now we wish to keep the current AST representation, in which
-- "__typename" is handled in a ad-hoc manner at each level in which
-- it can appear.
--
-- As a result, this temporary solution: __typename is manually added
-- to every selection set, which the function supplied as an argument
-- used to create the appropriate AST representation. The
-- `ifDefinitions` field of the fields parser is erased, to make the
-- field "invisible".
typenameField :: MonadParse m => (G.Name -> a) -> FieldsParser 'Output m (Maybe a)
typenameField f =
  (P.selection_ $$(G.litName "__typename") Nothing P.string `mapField` f)
    { ifDefinitions = []
    }
