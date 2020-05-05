module Hasura.GraphQL.Schema.Common where

import           Hasura.Prelude

import           Language.GraphQL.Draft.Syntax as G

import qualified Hasura.GraphQL.Parser         as P

import           Hasura.GraphQL.Parser         (FieldsParser, Kind (..))
import           Hasura.GraphQL.Parser.Class
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
-- Most selection sets accept the __typename keyword as a field,
-- which is simply expanded to the name of the type.
typenameField :: MonadParse m => (G.Name -> a) -> FieldsParser 'Output m (Maybe a)
typenameField f =
  P.selection_ $$(G.litName "__typename") Nothing P.string `mapField` f
