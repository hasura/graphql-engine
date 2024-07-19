-- | Implements /variable resolution/ for GraphQL queries, which annotates the
-- use site of each GraphQL variable with its value.
module Hasura.GraphQL.Execute.Resolve
  ( resolveVariables,
  )
where

import Data.Aeson qualified as J
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.HashSet qualified as HS
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.GraphQL.Parser.Variable
import Hasura.GraphQL.Transport.HTTP.Protocol qualified as GH
import Hasura.Prelude
import Hasura.RQL.Types.Schema.Options qualified as Options
import Language.GraphQL.Draft.Syntax qualified as G

-- | We must distinguish between a variable that is bound (because it either
-- has a value set by the user, or a default value regardless), and a nullable
-- variable with no user-given value and no default, and thus should be
-- unbound.
--
-- Why does this matter? Consider the following query with no supplied
-- variable values:
--
-- @
--     mutation($unset: String) {
--       update_author(_set: { name: $unset }) {
--         affected_rows
--       }
--     }
-- @
--
-- This should be a no-op returning @0@ rows. In order to be a no-op, we must
-- recognise that we can't just default its value to @null@, despite the fact
-- that it's a nullable variable. Thus, we "deliberately unbind" the variable,
-- resolve its occurrences to "deliberately unbound" resolutions, and then
-- bubble up the unboundness. This leaves us with something conceptually
-- equivalent to:
--
-- @
--     mutation($unset: String) {
--       update_author(_set: {}) {
--         affected_rows
--       }
--     }
-- @
--
-- ... which behaves as we'd like.
data Resolution = Bound Variable | DeliberatelyUnbound

-- | Convert a 'Resolution' to a 'Maybe'. Useful for 'mapMaybe' and the like.
resolvedToMaybe :: Resolution -> Maybe Variable
resolvedToMaybe = \case
  Bound var -> Just var
  DeliberatelyUnbound -> Nothing

resolveVariables ::
  forall m.
  (MonadError QErr m) =>
  Options.BackwardsCompatibleNullInNonNullableVariables ->
  Options.NoNullUnboundVariableDefault ->
  [G.VariableDefinition] ->
  GH.VariableValues ->
  [G.Directive G.Name] ->
  G.SelectionSet G.NoFragments G.Name ->
  m
    ( [G.Directive Variable],
      G.SelectionSet G.NoFragments Variable
    )
resolveVariables nullInNonNullableVariables noNullUnboundVariableDefault definitions jsonValues directives selSet = do
  variablesByNameList <- traverse buildVariable definitions

  let variablesByName :: HashMap G.Name (NonEmpty Resolution)
      variablesByName = HashMap.fromListWith (<>) do
        (name, def) <- variablesByNameList
        pure (name, pure def)

  uniqueVariables <- flip
    HashMap.traverseWithKey
    variablesByName
    \variableName variableDefinitions ->
      case variableDefinitions of
        a :| [] -> return a
        _ ->
          throw400 ParseFailed
            $ "multiple definitions for variable "
            <>> variableName
  ((directives', selSet'), usedVariables) <- flip runStateT mempty $ do
    d <- traverse (traverse (resolveVariable uniqueVariables)) directives
    s <- traverse (traverse (resolveVariable uniqueVariables)) selSet

    pure (map walkDirectives d, mapMaybe walkSelection s)

  let variablesByNameSet = HS.fromList . HashMap.keys $ variablesByName
      jsonVariableNames = HS.fromList $ HashMap.keys jsonValues
      -- At the time of writing, this check is disabled using
      -- a local binding because, the master branch doesn't implement this
      -- check.
      -- TODO: Do this check using a feature flag
      isVariableValidationEnabled = False

  when (isVariableValidationEnabled && usedVariables /= variablesByNameSet)
    $ throw400 ValidationFailed
    $ "following variable(s) have been defined, but have not been used in the query - "
    <> T.concat
      ( L.intersperse ", "
          $ map G.unName
          $ HS.toList
          $ HS.difference variablesByNameSet usedVariables
      )

  -- There may be variables which have a default value and may not be
  -- included in the variables JSON Map. So, we should only see, if a
  -- variable is inlcuded in the JSON Map, then it must be used in the
  -- query
  when (HS.difference jsonVariableNames usedVariables /= HS.empty)
    $ throw400 ValidationFailed
    $ "unexpected variables in variableValues: "
    <> T.concat
      ( L.intersperse ", "
          $ map G.unName
          $ HS.toList
          $ HS.difference jsonVariableNames usedVariables
      )

  return (directives', selSet')
  where
    -- Separating some noisy parts of @buildVariable@. If the computation
    -- "failed" to build a variable, it did so because we deliberately unbound
    -- it. Either way, we keep hold of the variable's name.
    run :: G.Name -> MaybeT m Variable -> m (G.Name, Resolution)
    run name (MaybeT xs) = do
      resolution <- fmap (maybe DeliberatelyUnbound Bound) xs
      pure (name, resolution)

    buildVariable :: G.VariableDefinition -> m (G.Name, Resolution)
    buildVariable G.VariableDefinition {G._vdName, G._vdType, G._vdDefaultValue} = run _vdName do
      let isOptional = isJust _vdDefaultValue || G.isNullable _vdType
      value <- case HashMap.lookup _vdName jsonValues of
        Just jsonValue ->
          -- If variable type is non-nullalbe , then raise exception if 'null' is provided
          if not (G.isNullable _vdType)
            -- `HASURA_GRAPHQL_BACKWARDS_COMPAT_NULL_IN_NONNULLABLE_VARIABLES` option set to `false`
            && nullInNonNullableVariables
            == Options.Don'tAllowNullInNonNullableVariables
            -- A 'null' value provided
            && jsonValue
            == J.Null
            then -- raise validation exception

              throw400 ValidationFailed
                $ "null value found for non-nullable type: "
                <>> G.showGT _vdType
            else pure $ Just $ JSONValue jsonValue
        Nothing
          | Just d <- _vdDefaultValue -> pure $ Just (GraphQLValue d)
          -- If the variable value was not provided, and the variable has no
          -- default value, then don't store a value for the variable.
          --
          -- Note that we hide this behaviour behind a feature flag to ensure
          -- backward compatibility.
          | G.isNullable _vdType -> case noNullUnboundVariableDefault of
              Options.DefaultUnboundNullableVariablesToNull -> pure Nothing
              Options.RemoveUnboundNullableVariablesFromTheQuery -> hoistMaybe Nothing
          | otherwise ->
              throw400 ValidationFailed
                $ "expecting a value for non-nullable variable: "
                <>> _vdName
      pure
        $! Variable
          { vInfo =
              if isOptional
                then VIOptional _vdName case noNullUnboundVariableDefault of
                  Options.RemoveUnboundNullableVariablesFromTheQuery -> _vdDefaultValue
                  Options.DefaultUnboundNullableVariablesToNull -> Just (fromMaybe G.VNull _vdDefaultValue)
                else VIRequired _vdName,
            vType = _vdType,
            vValue = value
          }
    resolveVariable :: HashMap G.Name Resolution -> G.Name -> StateT (HS.HashSet G.Name) m Resolution
    resolveVariable variables name = case HashMap.lookup name variables of
      Just (Bound variable) -> modify (HS.insert name) >> pure (Bound variable)
      Just DeliberatelyUnbound -> pure DeliberatelyUnbound
      Nothing -> throw400 ValidationFailed $ "unbound variable " <>> name

-- | Walk over a 'G.Value' to tidy up any variable 'Resolution'. Returns
-- 'Nothing' if the expression is entirely removed.
walkValue :: G.Value Resolution -> Maybe (G.Value Variable)
walkValue = \case
  G.VList xs -> Just (G.VList (mapMaybe walkValue xs))
  G.VObject xs -> Just (G.VObject (HashMap.mapMaybe walkValue xs))
  other -> traverse resolvedToMaybe other

-- | Walk over a 'G.Directive', removing any deliberately unbound arguments.
walkDirectives :: G.Directive Resolution -> G.Directive Variable
walkDirectives (G.Directive name args) = G.Directive name (HashMap.mapMaybe walkValue args)

-- | Walk over a 'G.Selection', removing any deliberately unbound arguments.
walkSelection :: G.Selection G.NoFragments Resolution -> Maybe (G.Selection G.NoFragments Variable)
walkSelection = \case
  G.SelectionField field ->
    Just
      $ G.SelectionField
        field
          { G._fArguments = HashMap.mapMaybe walkValue (G._fArguments field),
            G._fDirectives = map walkDirectives (G._fDirectives field),
            G._fSelectionSet = mapMaybe walkSelection (G._fSelectionSet field)
          }
  G.SelectionInlineFragment inlineFragment ->
    Just
      . G.SelectionInlineFragment
      $ inlineFragment
        { G._ifDirectives = map walkDirectives (G._ifDirectives inlineFragment),
          G._ifSelectionSet = mapMaybe walkSelection (G._ifSelectionSet inlineFragment)
        }
