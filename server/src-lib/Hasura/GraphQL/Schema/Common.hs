module Hasura.GraphQL.Schema.Common where

import           Hasura.Prelude

import           Language.GraphQL.Draft.Syntax (Name, litName, mkName)

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

mapField
  :: Functor m
  => P.FieldsParser k m (Maybe a)
  -> (a -> b)
  -> P.FieldsParser k m (Maybe b)
mapField fp f = fmap (fmap f) fp

numericAggOperators :: [Name]
numericAggOperators =
  [ $$(litName "sum")
  , $$(litName "avg")
  , $$(litName "stddev")
  , $$(litName "stddev_samp")
  , $$(litName "stddev_pop")
  , $$(litName "variance")
  , $$(litName "var_samp")
  , $$(litName "var_pop")
  ]

comparisonAggOperators :: [Name]
comparisonAggOperators = [$$(litName "max"), $$(litName "min")]
