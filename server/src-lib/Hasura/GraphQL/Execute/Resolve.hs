-- | Implements /variable resolution/ for GraphQL queries, which annotates the
-- use site of each GraphQL variable with its value.
module Hasura.GraphQL.Execute.Resolve
  ( resolveVariables
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended           as Map
import qualified Data.HashSet                           as HS
import qualified Data.List                              as L
import qualified Data.Text                              as T
import qualified Language.GraphQL.Draft.Syntax          as G

import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH

import           Hasura.GraphQL.Parser.Schema
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Types

resolveVariables
  :: forall m fragments
   . (MonadError QErr m, Traversable fragments)
  => [G.VariableDefinition]
  -> GH.VariableValues
  -> G.SelectionSet fragments G.Name
  -> m (G.SelectionSet fragments Variable)
resolveVariables definitions jsonValues selSet = do
  variablesByName <- Map.groupOnNE getName <$> traverse buildVariable definitions
  uniqueVariables <- flip Map.traverseWithKey variablesByName
    \variableName variableDefinitions ->
      case variableDefinitions of
        a :| [] -> return a
        _      -> throw400 ParseFailed
                   $ "multiple definitions for variable " <>> variableName
  (selSet', usedVariables) <- flip runStateT mempty $
    traverse (traverse (resolveVariable uniqueVariables)) selSet
  let variablesByNameSet = HS.fromList . Map.keys $ variablesByName
      jsonVariableNames = HS.fromList $ Map.keys jsonValues
      -- At the time of writing, this check is disabled using
      -- a local binding because, the master branch doesn't implement this
      -- check.
      -- TODO: Do this check using a feature flag
      isVariableValidationEnabled = False

  when (isVariableValidationEnabled && usedVariables /= variablesByNameSet) $
    throw400 ValidationFailed $
    "following variable(s) have been defined, but have not been used in the query - "
    <> T.concat (L.intersperse ", " $
                 map G.unName $ HS.toList $
                 HS.difference variablesByNameSet usedVariables)

  -- There may be variables which have a default value and may not be
  -- included in the variables JSON Map. So, we should only see, if a
  -- variable is inlcuded in the JSON Map, then it must be used in the
  -- query
  when (HS.difference jsonVariableNames usedVariables /= HS.empty) $
    throw400 ValidationFailed $
    "unexpected variables in variableValues: "
    <> T.concat (L.intersperse ", " $
                 map G.unName $ HS.toList $
                 HS.difference jsonVariableNames usedVariables)
  return selSet'
  where
    buildVariable :: G.VariableDefinition -> m Variable
    buildVariable G.VariableDefinition{ G._vdName, G._vdType, G._vdDefaultValue } = do
      let defaultValue = fromMaybe G.VNull _vdDefaultValue
      value <- case Map.lookup _vdName jsonValues of
        Just jsonValue -> pure $ JSONValue jsonValue
        Nothing
          | G.isNullable _vdType -> pure $ GraphQLValue $ absurd <$> defaultValue
          | otherwise -> throw400 ValidationFailed $
            "expecting a value for non-nullable variable: " <>> _vdName
      pure $! Variable
        { vInfo = if G.isNullable _vdType
            then VIOptional _vdName defaultValue
            else VIRequired _vdName
        , vType = _vdType
        , vValue = value
        }
    resolveVariable :: HashMap G.Name Variable -> G.Name -> StateT (HS.HashSet G.Name) m Variable
    resolveVariable variables name = case Map.lookup name variables of
      Just variable -> modify (HS.insert name) >> pure variable
      Nothing       -> throw400 ValidationFailed $ "unbound variable " <>> name
