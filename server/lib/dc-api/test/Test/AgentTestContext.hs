{-# LANGUAGE ConstraintKinds #-}

module Test.AgentTestContext
  ( AgentTestContext (..),
    introduceAgentTestContext,
    HasAgentTestContext,
    getAgentTestContext,
  )
where

import Command (AgentConfig)
import Control.Monad.Reader.Class (MonadReader)
import GHC.Stack (HasCallStack)
import Hasura.Backends.DataConnector.API qualified as API
import Test.Sandwich (HasLabel, Label (..), LabelValue, NodeOptions (..), SpecFree, defaultNodeOptions, getContext, introduce', type (:>))
import Prelude

-------------------------------------------------------------------------------

data AgentTestContext = AgentTestContext
  { _atcSourceName :: API.SourceName,
    _atcCapabilitiesResponse :: API.CapabilitiesResponse,
    -- | This is the configuration passed by the user on the command line which will
    -- be used in preference to any dataset clone's config if it is specified
    _atcAgentConfig :: AgentConfig
  }

introduceAgentTestContext :: forall context m. (Monad m) => AgentTestContext -> SpecFree (LabelValue "agent-test-context" AgentTestContext :> context) m () -> SpecFree context m ()
introduceAgentTestContext testContext =
  introduce' nodeOptions "Introduce agent test context" agentTestContextLabel (pure testContext) (const $ pure ())
  where
    nodeOptions =
      defaultNodeOptions
        { nodeOptionsVisibilityThreshold = 150,
          nodeOptionsCreateFolder = False,
          nodeOptionsRecordTime = False
        }

agentTestContextLabel :: Label "agent-test-context" AgentTestContext
agentTestContextLabel = Label

type HasAgentTestContext context = HasLabel context "agent-test-context" AgentTestContext

getAgentTestContext :: (HasCallStack, HasAgentTestContext context, MonadReader context m) => m AgentTestContext
getAgentTestContext = getContext agentTestContextLabel
