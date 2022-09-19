module Main (main) where

--------------------------------------------------------------------------------

import Command (AgentCapabilities (..), Command (..), TestOptions (..), parseCommandLine)
import Control.Exception (throwIO)
import Control.Monad (join, (>=>))
import Data.Aeson.Text (encodeToLazyText)
import Data.Fix (Fix (..), foldFix)
import Data.Foldable (for_, traverse_)
import Data.Proxy (Proxy (..))
import Data.Text.Lazy.IO qualified as Text
import Hasura.Backends.DataConnector.API (Routes (..), apiClient, openApiSchema)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.API.V0.Capabilities as API
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API (NamedRoutes)
import Servant.Client (Client, ClientError, hoistClient, mkClientEnv, runClientM, (//))
import Test.CapabilitiesSpec qualified
import Test.ExplainSpec qualified
import Test.HealthSpec qualified
import Test.Hspec (Spec)
import Test.Hspec.Core.Runner (evalSpec, runSpec)
import Test.Hspec.Core.Spec (Item (itemRequirement), SpecTree, Tree (..))
import Test.Hspec.Core.Util (filterPredicate)
import Test.Hspec.Runner (Config (..), Path, defaultConfig, evaluateSummary)
import Test.MetricsSpec qualified
import Test.QuerySpec qualified
import Test.SchemaSpec qualified
import Prelude

--------------------------------------------------------------------------------

testSourceName :: API.SourceName
testSourceName = "dc-api-tests"

tests :: Client IO (NamedRoutes Routes) -> API.SourceName -> API.Config -> API.Capabilities -> Spec
tests api sourceName agentConfig capabilities = do
  Test.HealthSpec.spec api sourceName agentConfig
  Test.CapabilitiesSpec.spec api agentConfig capabilities
  Test.SchemaSpec.spec api sourceName agentConfig
  Test.QuerySpec.spec api sourceName agentConfig capabilities
  for_ (API.cMetrics capabilities) \m -> Test.MetricsSpec.spec api m
  for_ (API.cExplain capabilities) \_ -> Test.ExplainSpec.spec api sourceName agentConfig capabilities

main :: IO ()
main = do
  command <- parseCommandLine
  case command of
    Test testOptions@TestOptions {..} -> do
      api <- mkIOApiClient testOptions
      agentCapabilities <- getAgentCapabilities api _toAgentCapabilities
      let spec = tests api testSourceName _toAgentConfig agentCapabilities
      case _toExportMatchStrings of
        False -> runSpec spec (applyTestConfig defaultConfig testOptions) >>= evaluateSummary
        True -> do
          tree <- fmap snd $ evalSpec defaultConfig spec
          traverse_ ((traverse_ putStrLn) . (foldPaths . extractLabels)) tree
    ExportOpenAPISpec ->
      Text.putStrLn $ encodeToLazyText openApiSchema

  pure ()

mkIOApiClient :: TestOptions -> IO (Client IO (NamedRoutes Routes))
mkIOApiClient TestOptions {..} = do
  manager <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager _toAgentBaseUrl
  pure $ hoistClient (Proxy @(NamedRoutes Routes)) (flip runClientM clientEnv >=> throwClientError) apiClient

throwClientError :: Either ClientError a -> IO a
throwClientError = either throwIO pure

getAgentCapabilities :: Client IO (NamedRoutes Routes) -> AgentCapabilities -> IO API.Capabilities
getAgentCapabilities api = \case
  AutoDetect -> API.crCapabilities <$> (api // _capabilities)
  Explicit capabilities -> pure capabilities

applyTestConfig :: Config -> TestOptions -> Config
applyTestConfig config TestOptions {..} =
  config
    { configConcurrentJobs = _toParallelDegree,
      configFilterPredicate = filterPredicate <$> _toMatch,
      configSkipPredicate = filterPredicates _toSkip,
      configDryRun = _toDryRun
    }

filterPredicates :: [String] -> Maybe (Path -> Bool)
filterPredicates [] = Nothing
filterPredicates xs = Just (\p -> any ($ p) (filterPredicate <$> xs))

--------------------------------------------------------------------------------

data TreeF r = NodeF String [r] | LeafF String
  deriving (Show, Functor)

-- | Fold a tree into a list of paths.
--
-- Note: we use @foldFix@ here because it is guaranteed to terminate and
-- bottom up recursion drastically simplifies the algorithm.
foldPaths :: Fix TreeF -> [String]
foldPaths = foldFix \case
  NodeF label paths -> fmap ((label <>) . ('/' :)) $ join paths
  LeafF label -> [label]

-- | Load the spec descriptions into a 'TreeF'
extractLabels :: SpecTree a -> Fix TreeF
extractLabels = \case
  Node s trs -> Fix $ NodeF s (fmap extractLabels trs)
  NodeWithCleanup ma _ trs -> Fix $ NodeF (foldMap fst ma) (fmap extractLabels trs)
  Leaf it -> Fix $ LeafF $ itemRequirement it
