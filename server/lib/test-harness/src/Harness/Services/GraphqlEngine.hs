-- | This module houses low-level functions and types to help access and work
-- with Graphql Engine.
module Harness.Services.GraphqlEngine
  ( withHge,
    withHgeSpawn,
    module ExternalHge,
    module HgeApi,
  )
where

import Control.Monad.Managed
import Data.Has
import Harness.Logging
import Harness.Services.Database.Postgres
import Harness.Services.ExternalProcess.GraphqlEngine as ExternalHge
import Harness.Services.GraphqlEngine.API as HgeApi
import Harness.Test.CustomOptions
import Harness.Yaml
import Hasura.Prelude
import Test.Hspec

-- | Draw a graphql-engine instance with specific environment variables set from
-- a pool of instances.
--
-- The logs emitted by the engine process are embedded in the test logs.
--
-- Note: The engine process will not be terminated if the test suite process
-- crashes. Ensuring that would require making Hge listen for heartbeats, or
-- use a helper process that does.
withHge ::
  ( Has HgeBinPath testEnvironment,
    Has PostgresServerUrl testEnvironment,
    Has Logger testEnvironment,
    Has PassthroughEnvVars testEnvironment,
    Has HgePool testEnvironment
  ) =>
  HgeConfig ->
  SpecWith (PostMetadata, (PostGraphql, (ShouldReturnYamlF, (HgeServerInstance, testEnvironment)))) ->
  SpecWith testEnvironment
withHge hgeConfig specs = do
  flip aroundWith specs \action testEnvironment -> runManaged do
    server <- drawFromPool testEnvironment hgeConfig
    liftIO
      $ action
        ( PostMetadata (hgePostMetadataWithStatusAndHeaders (server, testEnvironment)),
          ( PostGraphql (hgePostGraphqlWithHeaders (server, testEnvironment)),
            (ShouldReturnYamlF (shouldReturnYamlFInternal defaultOptions), (server, testEnvironment))
          )
        )

-- | Spawn a fresh graphql-engine instance with specific environment variables set.
--
-- For efficiency, prefer using 'withHge' instead of this function, unless the
-- pool somehow cannot work for your usecase.
withHgeSpawn ::
  ( Has HgeBinPath testEnvironment,
    Has PostgresServerUrl testEnvironment,
    Has Logger testEnvironment,
    Has PassthroughEnvVars testEnvironment
  ) =>
  HgeConfig ->
  SpecWith (PostMetadata, (PostGraphql, (ShouldReturnYamlF, (HgeServerInstance, testEnvironment)))) ->
  SpecWith testEnvironment
withHgeSpawn hgeConfig specs = do
  flip aroundWith specs \action testEnvironment -> do
    (server, cleanup) <- spawnServer testEnvironment hgeConfig
    action
      ( PostMetadata (hgePostMetadataWithStatusAndHeaders (server, testEnvironment)),
        ( PostGraphql (hgePostGraphqlWithHeaders (server, testEnvironment)),
          (ShouldReturnYamlF (shouldReturnYamlFInternal defaultOptions), (server, testEnvironment))
        )
      )
    cleanup
