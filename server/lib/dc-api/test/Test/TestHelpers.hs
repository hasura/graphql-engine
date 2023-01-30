module Test.TestHelpers
  ( AgentTestSpec,
    AgentClientTestSpec,
    AgentDatasetTestSpec,
    it,
  )
where

import Control.Monad.Free (Free)
import GHC.Stack (HasCallStack)
import Test.AgentClient (AgentClientT, HasAgentClient, runAgentClientT)
import Test.AgentDatasets (HasDatasetContext)
import Test.AgentTestContext (HasAgentTestContext)
import Test.Sandwich (ExampleT, HasBaseContext, SpecFree)
import Test.Sandwich qualified as Sandwich
import Test.Sandwich.Internal (SpecCommand)

type AgentTestSpec = forall context. (HasBaseContext context, HasAgentClient context, HasAgentTestContext context) => SpecFree context IO ()

type AgentClientTestSpec = forall context. (HasBaseContext context, HasAgentClient context) => SpecFree context IO ()

type AgentDatasetTestSpec = forall context. (HasBaseContext context, HasAgentTestContext context, HasDatasetContext context, HasAgentClient context) => SpecFree context IO ()

it :: (HasCallStack, HasAgentClient context, Monad m) => String -> AgentClientT (ExampleT context m) () -> Free (SpecCommand context m) ()
it label test = Sandwich.it label $ runAgentClientT Nothing test
