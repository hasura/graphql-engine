module Hasura.GraphQL.Execute.Remote
  ( buildExecStepRemote
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                    as Map
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Data.HashSet                           as Set

import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH

import           Hasura.GraphQL.Execute.Prepare
import           Hasura.RQL.Types

collectVariables
  :: forall fragments var
   . (Foldable fragments, Hashable var, Eq var)
  => G.SelectionSet fragments var
  -> Set.HashSet var
collectVariables =
  Set.unions . fmap (foldMap Set.singleton)

buildExecStepRemote
  :: forall db
   . RemoteSchemaInfo
  -> G.OperationType
  -> [G.VariableDefinition]
  -> G.SelectionSet G.NoFragments G.Name
  -> Maybe GH.VariableValues
  -> ExecutionStep db
buildExecStepRemote remoteSchemaInfo tp varDefs selSet varValsM =
  let requiredVars = collectVariables selSet
      restrictedDefs = filter (\varDef -> G._vdName varDef `Set.member` requiredVars) varDefs
      restrictedValsM = flip Map.intersection (Set.toMap requiredVars) <$> varValsM
  in ExecStepRemote (remoteSchemaInfo, G.TypedOperationDefinition tp Nothing restrictedDefs [] selSet, restrictedValsM)
