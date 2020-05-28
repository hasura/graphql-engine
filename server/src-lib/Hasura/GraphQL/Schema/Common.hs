module Hasura.GraphQL.Schema.Common where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.InsOrd    as OMap

import           Language.GraphQL.Draft.Syntax as G

import qualified Hasura.GraphQL.Parser         as P
import qualified Hasura.RQL.DML.Select.Types   as RQL

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
  => P.InputFieldsParser m (Maybe a)
  -> (a -> b)
  -> P.InputFieldsParser m (Maybe b)
mapField fp f = fmap (fmap f) fp

parsedSelectionsToFields
  :: (Text -> a) -- ^ how to handle @__typename@ fields
  -> OMap.InsOrdHashMap G.Name (P.ParsedSelection a)
  -> RQL.Fields a
parsedSelectionsToFields mkTypename = OMap.toList
  >>> map (FieldName . G.unName *** P.handleTypename (mkTypename . G.unName))

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
