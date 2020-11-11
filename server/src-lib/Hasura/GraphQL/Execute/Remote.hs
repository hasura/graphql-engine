module Hasura.GraphQL.Execute.Remote
  ( buildExecStepRemote
  , collectVariables
  , resolveRemoteVariable
  , resolveRemoteField
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Data.HashSet                           as Set
import qualified Data.HashMap.Strict                    as Map
import qualified Data.Text                              as T
import qualified Language.GraphQL.Draft.Syntax          as G

import           Data.Text.Extended

import           Hasura.GraphQL.Execute.Prepare
import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Context                 (RemoteFieldG (..), RemoteField)
import           Hasura.RQL.Types

import           Hasura.Session

mkVariableDefinitionAndValue :: Variable -> (G.VariableDefinition, (G.Name, J.Value))
mkVariableDefinitionAndValue var@(Variable varInfo gType varValue) =
  (varDefn, (varName, varJSONValue))
  where
    varName = getName var

    varDefn = G.VariableDefinition varName gType defaultVal

    defaultVal =
      case varInfo of
        VIRequired _ -> Nothing
        VIOptional _ val -> Just val

    varJSONValue =
      case varValue of
        JSONValue v -> v
        GraphQLValue val -> graphQLValueToJSON val

unresolveVariables
  :: forall fragments
   . Functor fragments
  => G.SelectionSet fragments Variable
  -> G.SelectionSet fragments G.Name
unresolveVariables =
  fmap (fmap (getName . vInfo))

collectVariables
  :: forall fragments var
   . (Foldable fragments, Hashable var, Eq var)
  => G.SelectionSet fragments var
  -> Set.HashSet var
collectVariables =
  Set.unions . fmap (foldMap Set.singleton)

buildExecStepRemote
  :: forall db
  .  RemoteSchemaInfo
  -> G.OperationType
  -> G.SelectionSet G.NoFragments Variable
  -> ExecutionStep db
buildExecStepRemote remoteSchemaInfo tp selSet =
  let unresolvedSelSet = unresolveVariables selSet
      allVars = map mkVariableDefinitionAndValue $ Set.toList $ collectVariables selSet
      varValues = Map.fromList $ map snd allVars
      varValsM = bool (Just varValues) Nothing $ Map.null varValues
      varDefs = map fst allVars
  in ExecStepRemote (remoteSchemaInfo, G.TypedOperationDefinition tp Nothing varDefs [] unresolvedSelSet, varValsM)

resolveRemoteVariable
  :: (MonadError QErr m)
  => UserInfo
  -> RemoteSchemaVariable
  -> m Variable
resolveRemoteVariable userInfo = \case
  SessionPresetVariable sessionVar gType varName presetType -> do
    sessionVarVal <- onNothing (getSessionVariableValue sessionVar $ _uiSession userInfo)
      $ throw400 NotFound $ sessionVar <<> " session variable expected, but not found"
    let baseType = G.getBaseType gType
        coercedValue =
          case presetType of
            SessionArgumentPresetScalar ->
              case G.unName baseType of
                "Int" -> G.VInt <$>
                  case readMaybe $ T.unpack sessionVarVal of
                    Nothing -> Left $ sessionVarVal <<> " cannot be coerced into an Int value"
                    Just i -> Right i
                "Boolean" ->
                  if | sessionVarVal `elem` ["true", "false"] -> Right $ G.VBoolean $ "true" == sessionVarVal
                     | otherwise -> Left $ sessionVarVal <<> " cannot be coerced into a Boolean value"
                "Float" -> G.VFloat <$>
                   case readMaybe $ T.unpack sessionVarVal of
                     Nothing -> Left $ sessionVarVal <<> " cannot be coerced into a Float value"
                     Just i -> Right i
                -- The `String`,`ID` and the default case all use the same code. But,
                -- it will be better to not merge all of them into the default case
                -- because it will be helpful to know how all the built-in scalars
                -- are handled
                "String" -> pure $ G.VString sessionVarVal
                "ID" -> pure $ G.VString sessionVarVal
                -- When we encounter a custom scalar, we just pass it as a string
                _ -> pure $ G.VString sessionVarVal
            SessionArgumentPresetEnum enumVals -> do
              sessionVarEnumVal <-
                G.EnumValue <$>
                  onNothing
                    (G.mkName sessionVarVal)
                    (Left $ sessionVarVal <<> " is not a valid GraphQL name")
              case find (== sessionVarEnumVal) enumVals of
                Just enumVal -> Right $ G.VEnum enumVal
                Nothing -> Left $ sessionVarEnumVal <<> " is not one of the valid enum values"
    coercedValue' <- onLeft coercedValue $ throw400 CoercionError
    pure $ Variable (VIRequired varName) gType (GraphQLValue coercedValue')
  RawVariable variable -> pure variable

resolveRemoteField
  :: (MonadError QErr m)
  => UserInfo
  -> RemoteField
  -> m (RemoteFieldG Variable)
resolveRemoteField userInfo = traverse (resolveRemoteVariable userInfo)
