module Test.TestHelpers
  ( AgentTestSpec,
    it,
  )
where

import Control.Monad.Free (Free)
import GHC.Stack (HasCallStack)
import Test.AgentClient (AgentClientT, HasAgentClient, runAgentClientT)
import Test.Sandwich (ExampleT, HasBaseContext, SpecFree)
import Test.Sandwich qualified as Sandwich
import Test.Sandwich.Internal (SpecCommand)

type AgentTestSpec = forall context. (HasBaseContext context, HasAgentClient context) => SpecFree context IO ()

it :: (HasCallStack, HasAgentClient context, Monad m) => String -> AgentClientT (ExampleT context m) () -> Free (SpecCommand context m) ()
it label test = Sandwich.it label $ runAgentClientT test
