module Hasura.GraphQL.Execute.Remote
  ( buildTypedOperation
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                    as Map
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Data.HashSet                           as Set

import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH

import           Hasura.GraphQL.Execute.Prepare
import           Hasura.GraphQL.Parser
import           Hasura.RQL.Types

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

buildTypedOperation
  :: forall db
   . RemoteSchemaInfo
  -> G.OperationType
  -> [G.VariableDefinition]
  -> G.SelectionSet G.NoFragments Variable
  -> Maybe GH.VariableValues
  -> ExecutionStep db
buildTypedOperation remoteSchemaInfo tp varDefs selSet varValsM =
  let unresolvedSelSet = unresolveVariables selSet
      requiredVars = collectVariables unresolvedSelSet
      restrictedDefs = filter (\varDef -> G._vdName varDef `Set.member` requiredVars) varDefs
      restrictedValsM = flip Map.intersection (Set.toMap requiredVars) <$> varValsM
  in ExecStepRemote (remoteSchemaInfo, G.TypedOperationDefinition tp Nothing restrictedDefs [] unresolvedSelSet, restrictedValsM)
