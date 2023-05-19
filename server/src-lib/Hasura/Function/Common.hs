module Hasura.Function.Common
  ( getFunctionAggregateGQLName,
    getFunctionArgsGQLName,
    getFunctionGQLName,
    getInputArgs,
  )
where

import Control.Lens
import Data.Sequence qualified as Seq
import Hasura.Function.Cache
import Hasura.Function.Lenses
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Language.GraphQL.Draft.Syntax qualified as G

-- | Apply function name customization to function arguments, as detailed in
-- 'rfcs/function-root-field-customisation.md'.  We want the different
-- variations of a function (i.e. basic, aggregate) to share the same type name
-- for their arguments.
getFunctionArgsGQLName ::
  -- | The GQL version of the DB name of the function
  G.Name ->
  FunctionConfig b ->
  -- | Custom function for setting naming case
  (G.Name -> G.Name) ->
  G.Name
getFunctionArgsGQLName
  funcGivenName
  FunctionConfig {..}
  setCase =
    setCase $ fromMaybe funcGivenName _fcCustomName <> Name.__args

-- | Apply function name customization to the basic function variation, as
-- detailed in 'rfcs/function-root-field-customisation.md'.
getFunctionGQLName ::
  G.Name ->
  FunctionConfig b ->
  -- | Custom function for setting naming case
  (G.Name -> G.Name) ->
  G.Name
getFunctionGQLName
  funcGivenName
  FunctionConfig
    { _fcCustomRootFields = FunctionCustomRootFields {..},
      ..
    }
  setCase =
    choice
      [ _fcrfFunction,
        _fcCustomName
      ]
      & fromMaybe (setCase funcGivenName)

-- | Apply function name customization to the aggregate function variation, as
-- detailed in 'rfcs/function-root-field-customisation.md'.
getFunctionAggregateGQLName ::
  G.Name ->
  FunctionConfig b ->
  -- | Custom function for setting naming case
  (G.Name -> G.Name) ->
  G.Name
getFunctionAggregateGQLName
  funcGivenName
  FunctionConfig
    { _fcCustomRootFields = FunctionCustomRootFields {..},
      ..
    }
  setCase =
    choice
      [ _fcrfFunctionAggregate,
        _fcCustomName <&> (<> Name.__aggregate)
      ]
      & fromMaybe (setCase $ funcGivenName <> Name.__aggregate)

getInputArgs :: FunctionInfo b -> Seq.Seq (FunctionArgument b)
getInputArgs =
  Seq.fromList . mapMaybe (^? _IAUserProvided) . toList . _fiInputArgs
