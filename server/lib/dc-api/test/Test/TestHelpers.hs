module Test.TestHelpers
  ( AgentTestSpec,
    AgentClientTestSpec,
    AgentDatasetTestSpec,
    it,
    edgeCaseTest,
  )
where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Free (Free)
import Control.Monad.IO.Class (MonadIO)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import Hasura.Backends.DataConnector.API qualified as API
import Test.AgentClient (AgentClientT, HasAgentClient, runAgentClientT)
import Test.AgentDatasets (HasDatasetContext, testingEdgeCasesTemplate, usesDataset)
import Test.AgentTestContext (HasAgentTestContext)
import Test.Data (EdgeCasesTestData (..))
import Test.Sandwich (ExampleT, HasBaseContext, SpecFree, pendingWith)
import Test.Sandwich qualified as Sandwich
import Test.Sandwich.Internal (SpecCommand)
import Prelude

type AgentTestSpec = forall context. (HasBaseContext context, HasAgentClient context, HasAgentTestContext context) => SpecFree context IO ()

type AgentClientTestSpec = forall context. (HasBaseContext context, HasAgentClient context) => SpecFree context IO ()

type AgentDatasetTestSpec = forall context. (HasBaseContext context, HasAgentTestContext context, HasDatasetContext context, HasAgentClient context) => SpecFree context IO ()

it :: (HasCallStack, HasAgentClient context, Monad m) => String -> AgentClientT (ExampleT context m) () -> Free (SpecCommand context m) ()
it label test = Sandwich.it label $ runAgentClientT Nothing test

edgeCaseTest ::
  (HasCallStack, HasAgentClient context, HasAgentTestContext context, HasBaseContext context, MonadThrow m, MonadIO m) =>
  Maybe EdgeCasesTestData ->
  (EdgeCasesTestData -> API.TableName) ->
  String ->
  (forall testContext. (HasBaseContext testContext, HasAgentClient testContext, HasAgentTestContext testContext, HasDatasetContext testContext) => EdgeCasesTestData -> AgentClientT (ExampleT testContext m) ()) ->
  Free (SpecCommand context m) ()
edgeCaseTest edgeCasesTestData expectedTable name test = do
  case edgeCasesTestData of
    Nothing -> it name $ pendingWith (testingEdgeCasesTemplateName <> " dataset template does not exist")
    Just edgeCasesTestData'@EdgeCasesTestData {..} ->
      if _ectdTableExists (expectedTable edgeCasesTestData')
        then usesDataset testingEdgeCasesTemplate $ it name $ test edgeCasesTestData'
        else it name $ pendingWith (Text.unpack (API.tableNameToText (expectedTable edgeCasesTestData')) <> " table does not exist within the " <> testingEdgeCasesTemplateName <> " dataset")
  where
    testingEdgeCasesTemplateName = Text.unpack (API._unDatasetTemplateName testingEdgeCasesTemplate)
