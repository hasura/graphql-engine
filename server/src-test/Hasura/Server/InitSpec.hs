module Hasura.Server.InitSpec
  ( spec,
  )
where

--------------------------------------------------------------------------------

import Data.HashSet qualified as Set
import Data.Monoid (All (..))
import Database.PG.Query qualified as Q
import Hasura.GraphQL.Execute.Subscription.Options qualified as ESO
import Hasura.GraphQL.Execute.Subscription.Options qualified as ESP
import Hasura.GraphQL.Schema.NamingCase qualified as NC
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.Logging qualified as Logging
import Hasura.Prelude
import Hasura.Server.Auth qualified as Auth
import Hasura.Server.Auth qualified as UUT
import Hasura.Server.Cors qualified as Cors
import Hasura.Server.Init qualified as UUT
import Hasura.Server.Logging qualified as Logging
import Hasura.Server.Types qualified as Types
import Hasura.Session qualified as UUT
import Network.WebSockets qualified as WS
import Test.Hspec qualified as Hspec

--------------------------------------------------------------------------------

spec :: Hspec.Spec
spec = Hspec.describe "Init Tests" $ do
  mkServeOptionsSpec

--------------------------------------------------------------------------------

emptyRawServeOptions :: UUT.RawServeOptions Logging.Hasura
emptyRawServeOptions =
  UUT.RawServeOptions
    { rsoPort = Nothing,
      rsoHost = Nothing,
      rsoConnParams =
        UUT.RawConnParams
          { rcpStripes = Nothing,
            rcpConns = Nothing,
            rcpIdleTime = Nothing,
            rcpConnLifetime = Nothing,
            rcpAllowPrepare = Nothing,
            rcpPoolTimeout = Nothing
          },
      rsoTxIso = Nothing,
      rsoAdminSecret = Nothing,
      rsoAuthHook = UUT.AuthHookG Nothing Nothing,
      rsoJwtSecret = Nothing,
      rsoUnAuthRole = Nothing,
      rsoCorsConfig = Nothing,
      rsoEnableConsole = False,
      rsoConsoleAssetsDir = Nothing,
      rsoEnableTelemetry = Nothing,
      rsoWsReadCookie = False,
      rsoStringifyNum = False,
      rsoDangerousBooleanCollapse = Nothing,
      rsoEnabledAPIs = Nothing,
      rsoMxRefetchInt = Nothing,
      rsoMxBatchSize = Nothing,
      rsoStreamingMxRefetchInt = Nothing,
      rsoStreamingMxBatchSize = Nothing,
      rsoEnableAllowlist = False,
      rsoEnabledLogTypes = Nothing,
      rsoLogLevel = Nothing,
      rsoDevMode = False,
      rsoAdminInternalErrors = Nothing,
      rsoEventsHttpPoolSize = Nothing,
      rsoEventsFetchInterval = Nothing,
      rsoAsyncActionsFetchInterval = Nothing,
      rsoEnableRemoteSchemaPermissions = False,
      rsoWebSocketCompression = False,
      rsoWebSocketKeepAlive = Nothing,
      rsoInferFunctionPermissions = Nothing,
      rsoEnableMaintenanceMode = False,
      rsoSchemaPollInterval = Nothing,
      rsoExperimentalFeatures = Nothing,
      rsoEventsFetchBatchSize = Nothing,
      rsoGracefulShutdownTimeout = Nothing,
      rsoWebSocketConnectionInitTimeout = Nothing,
      rsoEnableMetadataQueryLoggingEnv = False,
      rsoDefaultNamingConvention = Nothing
    }

mkServeOptionsSpec :: Hspec.Spec
mkServeOptionsSpec =
  Hspec.describe "mkServeOptions" $ do
    Hspec.describe "soPort" $ do
      Hspec.it "Default == 8080" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soPort result `Hspec.shouldSatisfy` \case
          Right port -> port == 8080
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_SERVER_PORT", "420")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soPort result `Hspec.shouldSatisfy` \case
          Right port -> port == 420
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoPort = Just 11}
            -- When
            env = [("HASURA_GRAPHQL_SERVER_PORT", "420")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soPort result `Hspec.shouldSatisfy` \case
          Right port -> port == 11
          Left _err -> False

    Hspec.describe "soHost" $ do
      Hspec.it "Default = '*'" $ do
        -- Given
        let rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soHost result `Hspec.shouldSatisfy` \case
          Right host -> host == "*"
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        -- Given
        let rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_SERVER_HOST", "127.0.0.1")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soHost result `Hspec.shouldSatisfy` \case
          Right host -> host == "127.0.0.1"
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        -- Given
        let rawServeOptions = emptyRawServeOptions {UUT.rsoHost = Just "*4"}
            -- When
            env = [("HASURA_GRAPHQL_SERVER_HOST", "127.0.0.1")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soHost result `Hspec.shouldSatisfy` \case
          Right host -> host == "*4"
          Left _err -> False

    Hspec.describe "soConnParams" $ do
      Hspec.it "Default == 1, 50, 180, 600" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soConnParams result `Hspec.shouldSatisfy` \case
          Right connParams ->
            getAll $
              foldMap
                All
                [ Q.cpStripes connParams == 1,
                  Q.cpConns connParams == 50,
                  Q.cpIdleTime connParams == 180,
                  Q.cpAllowPrepare connParams == True,
                  Q.cpMbLifetime connParams == Just 600,
                  Q.cpTimeout connParams == Nothing,
                  Q.cpCancel connParams == True
                ]
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env =
              [ ("HASURA_GRAPHQL_PG_STRIPES", "42"),
                ("HASURA_GRAPHQL_PG_CONNECTIONS", "43"),
                ("HASURA_GRAPHQL_PG_TIMEOUT", "44"),
                ("HASURA_GRAPHQL_PG_CONN_LIFETIME", "45"),
                ("HASURA_GRAPHQL_USE_PREPARED_STATEMENTS", "false"),
                ("HASURA_GRAPHQL_PG_POOL_TIMEOUT", "46")
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soConnParams result `Hspec.shouldSatisfy` \case
          Right connParams ->
            getAll $
              foldMap
                All
                [ Q.cpStripes connParams == 42,
                  Q.cpConns connParams == 43,
                  Q.cpIdleTime connParams == 44,
                  Q.cpAllowPrepare connParams == False,
                  Q.cpMbLifetime connParams == Just 45,
                  Q.cpTimeout connParams == Just 46,
                  Q.cpCancel connParams == True
                ]
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions =
              emptyRawServeOptions
                { UUT.rsoConnParams =
                    UUT.RawConnParams
                      { rcpStripes = Just 2,
                        rcpConns = Just 3,
                        rcpIdleTime = Just 4,
                        rcpConnLifetime = Just 4,
                        rcpAllowPrepare = Just True,
                        rcpPoolTimeout = Just 5
                      }
                }
            -- When
            env =
              [ ("HASURA_GRAPHQL_PG_STRIPES", "42"),
                ("HASURA_GRAPHQL_PG_CONNECTIONS", "43"),
                ("HASURA_GRAPHQL_PG_TIMEOUT", "44"),
                ("HASURA_GRAPHQL_PG_CONN_LIFETIME", "45"),
                ("HASURA_GRAPHQL_USE_PREPARED_STATEMENTS", "false"),
                ("HASURA_GRAPHQL_PG_POOL_TIMEOUT", "46")
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soConnParams result `Hspec.shouldSatisfy` \case
          Right connParams ->
            getAll $
              foldMap
                All
                [ Q.cpStripes connParams == 2,
                  Q.cpConns connParams == 3,
                  Q.cpIdleTime connParams == 4,
                  Q.cpAllowPrepare connParams == True,
                  Q.cpMbLifetime connParams == Just 4,
                  Q.cpTimeout connParams == Just 5,
                  Q.cpCancel connParams == True
                ]
          Left _err -> False

    Hspec.describe "soTxIso" $ do
      Hspec.it "Default == ReadCommitted" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soTxIso result `Hspec.shouldSatisfy` \case
          Right txIso -> txIso == Q.ReadCommitted
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_TX_ISOLATION", "repeatable-read")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soTxIso result `Hspec.shouldSatisfy` \case
          Right txIso -> txIso == Q.RepeatableRead
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        -- Given
        let rawServeOptions = emptyRawServeOptions {UUT.rsoTxIso = Just Q.Serializable}
            -- When
            env = [("HASURA_GRAPHQL_TX_ISOLATION", "repeatable-read")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soTxIso result `Hspec.shouldSatisfy` \case
          Right txIso -> txIso == Q.Serializable
          Left _err -> False

    Hspec.describe "soAdminSecret" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_ADMIN_SECRET", "A monad is a monoid in the category of endofunctors")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soAdminSecret result `Hspec.shouldSatisfy` \case
          Right soAdminSecret -> soAdminSecret == Set.singleton (Auth.hashAdminSecret "A monad is a monoid in the category of endofunctors")
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        -- Given
        let rawServeOptions = emptyRawServeOptions {UUT.rsoAdminSecret = Just (Auth.hashAdminSecret "Whats the big deal")}
            -- When
            env = [("HASURA_GRAPHQL_ADMIN_SECRET", "A monad is a monoid in the category of endofunctors")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soAdminSecret result `Hspec.shouldSatisfy` \case
          Right soAdminSecret -> soAdminSecret == Set.singleton (Auth.hashAdminSecret "Whats the big deal")
          Left _err -> False

    Hspec.describe "soAuthHook" $ do
      Hspec.it "Default Hook Mode == GET" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_AUTH_HOOK", "http://auth.hook.com")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soAuthHook result `Hspec.shouldSatisfy` \case
          Right soAuthHook -> soAuthHook == Just (UUT.AuthHookG "http://auth.hook.com" UUT.AHTGet)
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env =
              [ ("HASURA_GRAPHQL_AUTH_HOOK", "http://auth.hook.com"),
                ("HASURA_GRAPHQL_AUTH_HOOK_MODE", "POST")
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soAuthHook result `Hspec.shouldSatisfy` \case
          Right soAuthHook -> soAuthHook == Just (UUT.AuthHookG "http://auth.hook.com" UUT.AHTPost)
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoAuthHook = UUT.AuthHookG (Just "http://auth.hook.com") (Just UUT.AHTGet)}
            -- When
            env =
              [ ("HASURA_GRAPHQL_AUTH_HOOK", "http://auth.hook.net"),
                ("HASURA_GRAPHQL_AUTH_HOOK_MODE", "POST")
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soAuthHook result `Hspec.shouldSatisfy` \case
          Right soAuthHook -> soAuthHook == Just (UUT.AuthHookG "http://auth.hook.com" UUT.AHTGet)
          Left _err -> False

    Hspec.describe "soJwtSecret" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env =
              [ ( "HASURA_GRAPHQL_JWT_SECRET",
                  "{\"type\": \"HS256\", \"key\": \"11111111111111111111111111111111\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}"
                )
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soJwtSecret result `Hspec.shouldSatisfy` \case
          Right soJwtSecret ->
            Right soJwtSecret
              == UUT.fromEnv "[{\"type\": \"HS256\", \"key\": \"11111111111111111111111111111111\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}]"
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            jwtConfig = eitherToMaybe $ UUT.fromEnv @UUT.JWTConfig "{\"type\": \"HS256\", \"key\": \"22222222222222222222222222222222\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}"
            rawServeOptions = emptyRawServeOptions {UUT.rsoJwtSecret = jwtConfig}
            -- When
            env =
              [ ( "HASURA_GRAPHQL_JWT_SECRET",
                  "{\"type\": \"HS256\", \"key\": \"11111111111111111111111111111111\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}"
                )
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soJwtSecret result `Hspec.shouldSatisfy` \case
          Right soJwtSecret -> soJwtSecret == maybe [] pure jwtConfig
          Left _err -> False

    Hspec.describe "soUnAuthRole" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_UNAUTHORIZED_ROLE", "guest")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soUnAuthRole result `Hspec.shouldSatisfy` \case
          Right soUnAuthRole -> soUnAuthRole == UUT.mkRoleName "guest"
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoUnAuthRole = UUT.mkRoleName "visitor"}
            -- When
            env = [("HASURA_GRAPHQL_UNAUTHORIZED_ROLE", "guest")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soUnAuthRole result `Hspec.shouldSatisfy` \case
          Right soUnAuthRole -> soUnAuthRole == UUT.mkRoleName "visitor"
          Left _err -> False

    Hspec.describe "soCorsConfig" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_CORS_DOMAIN", "http://domain1:23, http://domain2:34")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soCorsConfig result `Hspec.shouldSatisfy` \case
          Right soCorsConfig -> Just soCorsConfig == eitherToMaybe (Cors.readCorsDomains "http://domain1:23, http://domain2:34")
          Left _err -> False

      Hspec.it "Default CorsConfig == CCAllowAll" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soCorsConfig result `Hspec.shouldSatisfy` \case
          Right soCorsConfig -> soCorsConfig == Cors.CCAllowAll
          Left _err -> False

      Hspec.it "Env 'HASURA_GRAPHQL_DISABLE_CORS=false' superseded by 'HASURA_GRAPHQL_CORS_DOMAIN'" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_CORS_DOMAIN", "http://domain1:23, http://domain2:34"), ("HASURA_GRAPHQL_DISABLE_CORS", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soCorsConfig result `Hspec.shouldSatisfy` \case
          Right soCorsConfig -> Just soCorsConfig == eitherToMaybe (Cors.readCorsDomains "http://domain1:23, http://domain2:34")
          Left _err -> False

      Hspec.it "Env 'HASURA_GRAPHQL_DISABLE_CORS=true' enables use of cookie value" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_WS_READ_COOKIE", "true"), ("HASURA_GRAPHQL_DISABLE_CORS", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soCorsConfig result `Hspec.shouldSatisfy` \case
          Right soCorsConfig -> soCorsConfig == Cors.CCDisabled True
          Left _err -> False

      Hspec.it "Env 'HASURA_GRAPHQL_DISABLE_CORS=true' supersedes 'HASURA_GRAPHQL_CORS_DOMAIN'" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_CORS_DOMAIN", "http://domain1:23, http://domain2:34"), ("HASURA_GRAPHQL_DISABLE_CORS", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soCorsConfig result `Hspec.shouldSatisfy` \case
          Right soCorsConfig -> soCorsConfig == Cors.CCDisabled False
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoCorsConfig = eitherToMaybe (Cors.readCorsDomains "http://domain1:23, http://domain2:34")}
            -- When
            env = [("HASURA_GRAPHQL_CORS_DOMAIN", "http://domain3:23, http://domain4:34")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soCorsConfig result `Hspec.shouldSatisfy` \case
          Right soCorsConfig -> Just soCorsConfig == eitherToMaybe (Cors.readCorsDomains "http://domain1:23, http://domain2:34")
          Left _err -> False

    Hspec.describe "soEnableConsole" $ do
      Hspec.it "Default == False" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableConsole result `Hspec.shouldSatisfy` \case
          Right soEnableConsole -> soEnableConsole == False
          Left _err -> False

      -- NOTE: This is a little confusing. We are first simulating
      -- not providing the '--enable-console' flag with the env var set to 'true'.
      -- Next we simulate providing the '--enable-console' flag and
      -- the env var to ensure the flag supersedes the env var.
      Hspec.it "Env > No Switch" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_CONSOLE", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableConsole result `Hspec.shouldSatisfy` \case
          Right soEnableConsole -> soEnableConsole == True
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoEnableConsole = True}
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_CONSOLE", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableConsole result `Hspec.shouldSatisfy` \case
          Right soEnableConsole -> soEnableConsole == True
          Left _err -> False

    Hspec.describe "soConsoleAssetsDir" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_CONSOLE_ASSETS_DIR", "/assets")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soConsoleAssetsDir result `Hspec.shouldSatisfy` \case
          Right soConsoleAssetsDir -> soConsoleAssetsDir == Just "/assets"
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoConsoleAssetsDir = Just "/data"}
            -- When
            env = [("HASURA_GRAPHQL_CONSOLE_ASSETS_DIR", "/assets")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soConsoleAssetsDir result `Hspec.shouldSatisfy` \case
          Right soConsoleAssetsDir -> soConsoleAssetsDir == Just "/data"
          Left _err -> False

    Hspec.describe "soEnableTelemetry" $ do
      Hspec.it "Default == True" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableTelemetry result `Hspec.shouldSatisfy` \case
          Right soEnableTelemetry -> soEnableTelemetry == True
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_TELEMETRY", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableTelemetry result `Hspec.shouldSatisfy` \case
          Right soEnableTelemetry -> soEnableTelemetry == False
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoEnableTelemetry = Just False}
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_TELEMETRY", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableTelemetry result `Hspec.shouldSatisfy` \case
          Right soEnableTelemetry -> soEnableTelemetry == False
          Left _err -> False

    Hspec.describe "soStringifyNum" $ do
      Hspec.it "Default == LeaveNumbersAlone" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soStringifyNum result `Hspec.shouldSatisfy` \case
          Right soStringifyNum -> soStringifyNum == Options.Don'tStringifyNumbers
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soStringifyNum result `Hspec.shouldSatisfy` \case
          Right soStringifyNum -> soStringifyNum == Options.StringifyNumbers
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoStringifyNum = True}
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_TELEMETRY", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soStringifyNum result `Hspec.shouldSatisfy` \case
          Right soStringifyNum -> soStringifyNum == Options.StringifyNumbers
          Left _err -> False

    Hspec.describe "soDangerousBooleanCollapse" $ do
      Hspec.it "Default == False" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soDangerousBooleanCollapse result `Hspec.shouldSatisfy` \case
          Right soDangerousBooleanCollapse -> soDangerousBooleanCollapse == False
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_V1_BOOLEAN_NULL_COLLAPSE", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soDangerousBooleanCollapse result `Hspec.shouldSatisfy` \case
          Right soDangerousBooleanCollapse -> soDangerousBooleanCollapse == True
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoDangerousBooleanCollapse = Just False}
            -- When
            env = [("HASURA_GRAPHQL_V1_BOOLEAN_NULL_COLLAPSE", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soDangerousBooleanCollapse result `Hspec.shouldSatisfy` \case
          Right soDangerousBooleanCollapse -> soDangerousBooleanCollapse == False
          Left _err -> False

    Hspec.describe "soEnabledAPIs" $ do
      Hspec.it "Default == metadata,graphql,pgdump,config" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnabledAPIs result `Hspec.shouldSatisfy` \case
          Right soEnabledAPIs -> soEnabledAPIs == Set.fromList [UUT.METADATA, UUT.GRAPHQL, UUT.PGDUMP, UUT.CONFIG]
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_ENABLED_APIS", "metadata,graphql")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnabledAPIs result `Hspec.shouldSatisfy` \case
          Right soEnabledAPIs -> soEnabledAPIs == Set.fromList [UUT.METADATA, UUT.GRAPHQL]
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoEnabledAPIs = Just [UUT.CONFIG]}
            -- When
            env = [("HASURA_GRAPHQL_ENABLED_APIS", "metadata,graphql")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnabledAPIs result `Hspec.shouldSatisfy` \case
          Right soEnabledAPIs -> soEnabledAPIs == Set.fromList [UUT.CONFIG]
          Left _err -> False

    Hspec.describe "soLiveQueryOpts" $ do
      Hspec.it "Default == 1000, 100" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soLiveQueryOpts result `Hspec.shouldSatisfy` \case
          Right soLiveQueryOpts ->
            getAll $
              foldMap
                All
                [ Just (ESO._lqoRefetchInterval soLiveQueryOpts) == ESO.mkRefetchInterval 1,
                  Just (ESO._lqoBatchSize soLiveQueryOpts) == ESO.mkBatchSize 100
                ]
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env =
              [ ("HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_REFETCH_INTERVAL", "2000"),
                ("HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_BATCH_SIZE", "200")
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soLiveQueryOpts result `Hspec.shouldSatisfy` \case
          Right soLiveQueryOpts ->
            getAll $
              foldMap
                All
                [ Just (ESO._lqoRefetchInterval soLiveQueryOpts) == ESO.mkRefetchInterval 2,
                  Just (ESO._lqoBatchSize soLiveQueryOpts) == ESO.mkBatchSize 200
                ]
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions =
              emptyRawServeOptions
                { UUT.rsoMxRefetchInt = ESO.mkRefetchInterval 3,
                  UUT.rsoMxBatchSize = ESP.mkBatchSize 300
                }
            -- When
            env =
              [ ("HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_REFETCH_INTERVAL", "2000"),
                ("HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_BATCH_SIZE", "200")
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soLiveQueryOpts result `Hspec.shouldSatisfy` \case
          Right soLiveQueryOpts ->
            getAll $
              foldMap
                All
                [ Just (ESO._lqoRefetchInterval soLiveQueryOpts) == ESO.mkRefetchInterval 3,
                  Just (ESO._lqoBatchSize soLiveQueryOpts) == ESO.mkBatchSize 300
                ]
          Left _err -> False

    Hspec.describe "soStreamingQueryOpts" $ do
      Hspec.it "Default == 1000, 100" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soStreamingQueryOpts result `Hspec.shouldSatisfy` \case
          Right soStreamingQueryOpts ->
            getAll $
              foldMap
                All
                [ Just (ESO._lqoRefetchInterval soStreamingQueryOpts) == ESO.mkRefetchInterval 1,
                  Just (ESO._lqoBatchSize soStreamingQueryOpts) == ESO.mkBatchSize 100
                ]
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env =
              [ ("HASURA_GRAPHQL_STREAMING_QUERIES_MULTIPLEXED_REFETCH_INTERVAL", "2000"),
                ("HASURA_GRAPHQL_STREAMING_QUERIES_MULTIPLEXED_BATCH_SIZE", "200")
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soStreamingQueryOpts result `Hspec.shouldSatisfy` \case
          Right soStreamingQueryOpts ->
            getAll $
              foldMap
                All
                [ Just (ESO._lqoRefetchInterval soStreamingQueryOpts) == ESO.mkRefetchInterval 2,
                  Just (ESO._lqoBatchSize soStreamingQueryOpts) == ESO.mkBatchSize 200
                ]
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions =
              emptyRawServeOptions
                { UUT.rsoStreamingMxRefetchInt = ESO.mkRefetchInterval 3,
                  UUT.rsoStreamingMxBatchSize = ESO.mkBatchSize 300
                }
            -- When
            env =
              [ ("HASURA_GRAPHQL_STREAMING_QUERIES_MULTIPLEXED_REFETCH_INTERVAL", "2000"),
                ("HASURA_GRAPHQL_STREAMING_QUERIES_MULTIPLEXED_BATCH_SIZE", "200")
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soStreamingQueryOpts result `Hspec.shouldSatisfy` \case
          Right soStreamingQueryOpts ->
            getAll $
              foldMap
                All
                [ Just (ESO._lqoRefetchInterval soStreamingQueryOpts) == ESO.mkRefetchInterval 3,
                  Just (ESO._lqoBatchSize soStreamingQueryOpts) == ESO.mkBatchSize 300
                ]
          Left _err -> False

    Hspec.describe "soEnableAllowlist" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_ALLOWLIST", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableAllowlist result `Hspec.shouldSatisfy` \case
          Right soEnableAllowlist -> soEnableAllowlist == True
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoEnableAllowlist = True}
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_ALLOWLIST", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableAllowlist result `Hspec.shouldSatisfy` \case
          Right soEnableAllowlist -> soEnableAllowlist == True
          Left _err -> False

    Hspec.describe "soEnabledLogTypes" $ do
      Hspec.it "Default == Startup, HttpLog, WebhookLog, WebsocketLog" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnabledLogTypes result `Hspec.shouldSatisfy` \case
          Right soEnabledLogTypes ->
            soEnabledLogTypes == Set.fromList [Logging.ELTStartup, Logging.ELTHttpLog, Logging.ELTWebhookLog, Logging.ELTWebsocketLog]
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_ENABLED_LOG_TYPES", "http-log")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnabledLogTypes result `Hspec.shouldSatisfy` \case
          Right soEnabledLogTypes -> soEnabledLogTypes == Set.fromList [Logging.ELTHttpLog]
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoEnabledLogTypes = Just [Logging.ELTActionHandler]}
            -- When
            env = [("HASURA_GRAPHQL_ENABLED_LOG_TYPES", "http-log")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnabledLogTypes result `Hspec.shouldSatisfy` \case
          Right soEnabledLogTypes -> soEnabledLogTypes == Set.fromList [Logging.ELTActionHandler]
          Left _err -> False

    Hspec.describe "soLogLevel" $ do
      Hspec.it "Default == LevelInfo" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soLogLevel result `Hspec.shouldSatisfy` \case
          Right soLogLevel -> soLogLevel == Logging.LevelInfo
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_LOG_LEVEL", "warn")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soLogLevel result `Hspec.shouldSatisfy` \case
          Right soLogLevel -> soLogLevel == Logging.LevelWarn
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoLogLevel = Just Logging.LevelWarn}
            -- When
            env = [("HASURA_GRAPHQL_LOG_LEVEL", "warn")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soLogLevel result `Hspec.shouldSatisfy` \case
          Right soLogLevel -> soLogLevel == Logging.LevelWarn
          Left _err -> False

    Hspec.describe "soDevMode" $ do
      Hspec.it "Default == False" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soDevMode &&& UUT.soResponseInternalErrorsConfig) result `Hspec.shouldSatisfy` \case
          Right (soDevMode, soResponseInternalErrorsConfig) ->
            getAll $ foldMap All [soDevMode == False, soResponseInternalErrorsConfig == UUT.InternalErrorsAdminOnly]
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_DEV_MODE", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soDevMode &&& UUT.soResponseInternalErrorsConfig) result `Hspec.shouldSatisfy` \case
          Right (soDevMode, soResponseInternalErrorsConfig) ->
            getAll $ foldMap All [soDevMode == True, soResponseInternalErrorsConfig == UUT.InternalErrorsAllRequests]
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoDevMode = True}
            -- When
            env = [("HASURA_GRAPHQL_DEV_MODE", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soDevMode result `Hspec.shouldSatisfy` \case
          Right soDevMode -> soDevMode == True
          Left _err -> False

    Hspec.describe "soAdminInternalErrors" $ do
      Hspec.it "Default == InternalErrorsAdminOnly" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soResponseInternalErrorsConfig result `Hspec.shouldSatisfy` \case
          Right soResponseInternalErrorsConfig -> soResponseInternalErrorsConfig == UUT.InternalErrorsAdminOnly
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_ADMIN_INTERNAL_ERRORS", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soResponseInternalErrorsConfig result `Hspec.shouldSatisfy` \case
          Right soResponseInternalErrorsConfig -> soResponseInternalErrorsConfig == UUT.InternalErrorsDisabled
          Left _err -> False

      Hspec.it "Dev Mode supersedes rsoAdminInternalErrors" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_ADMIN_INTERNAL_ERRORS", "false"), ("HASURA_GRAPHQL_DEV_MODE", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soResponseInternalErrorsConfig result `Hspec.shouldSatisfy` \case
          Right soResponseInternalErrorsConfig -> soResponseInternalErrorsConfig == UUT.InternalErrorsAllRequests
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoAdminInternalErrors = Just False}
            -- When
            env = [("HASURA_GRAPHQL_ADMIN_INTERNAL_ERRORS", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soResponseInternalErrorsConfig result `Hspec.shouldSatisfy` \case
          Right soResponseInternalErrorsConfig -> soResponseInternalErrorsConfig == UUT.InternalErrorsDisabled
          Left _err -> False

    Hspec.describe "soEventsHttpPoolSize" $ do
      -- TODO(SOLOMON): Despite the parser help message, there
      -- actually isn't a default value set in 'ServeOptions' for
      -- this:
      --Hspec.it "Default == 100" $ do
      --  let -- Given
      --      rawServeOptions = emptyRawServeOptions
      --      -- When
      --      env = []
      --      -- Then
      --      result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

      --  fmap UUT.soEventsHttpPoolSize result `Hspec.shouldSatisfy` \case
      --    Right soEventsHttpPoolSize -> soEventsHttpPoolSize == Just 100
      --    Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEventsHttpPoolSize result `Hspec.shouldSatisfy` \case
          Right soEventsHttpPoolSize -> soEventsHttpPoolSize == Just 200
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoEventsHttpPoolSize = Just 300}
            -- When
            env = [("HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEventsHttpPoolSize result `Hspec.shouldSatisfy` \case
          Right soEventsHttpPoolSize -> soEventsHttpPoolSize == Just 300
          Left _err -> False

    Hspec.describe "soEventsFetchInterval" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEventsFetchInterval result `Hspec.shouldSatisfy` \case
          Right soEventsFetchInterval -> soEventsFetchInterval == Just 200
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoEventsFetchInterval = Just 300}
            -- When
            env = [("HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEventsFetchInterval result `Hspec.shouldSatisfy` \case
          Right soEventsFetchInterval -> soEventsFetchInterval == Just 300
          Left _err -> False

    Hspec.describe "soAsyncActionsFetchInterval" $ do
      Hspec.it "Default == 1000" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soAsyncActionsFetchInterval result `Hspec.shouldSatisfy` \case
          Right soAsyncActionsFetchInterval -> soAsyncActionsFetchInterval == UUT.Interval 1000
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_ASYNC_ACTIONS_FETCH_INTERVAL", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soAsyncActionsFetchInterval result `Hspec.shouldSatisfy` \case
          Right soAsyncActionsFetchInterval -> soAsyncActionsFetchInterval == UUT.Interval 200
          Left _err -> False

      Hspec.it "0 == 'Skip'" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_ASYNC_ACTIONS_FETCH_INTERVAL", "0")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soAsyncActionsFetchInterval result `Hspec.shouldSatisfy` \case
          Right soAsyncActionsFetchInterval -> soAsyncActionsFetchInterval == UUT.Skip
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoAsyncActionsFetchInterval = Just 300}
            -- When
            env = [("HASURA_GRAPHQL_ASYNC_ACTIONS_FETCH_INTERVAL", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soAsyncActionsFetchInterval result `Hspec.shouldSatisfy` \case
          Right soAsyncActionsFetchInterval -> soAsyncActionsFetchInterval == UUT.Interval 300
          Left _err -> False

    Hspec.describe "soEnableRemoteSchemaPermissions" $ do
      Hspec.it "Default == False" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableRemoteSchemaPermissions result `Hspec.shouldSatisfy` \case
          Right soEnableRemoteSchemaPermissions -> soEnableRemoteSchemaPermissions == Options.DisableRemoteSchemaPermissions
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableRemoteSchemaPermissions result `Hspec.shouldSatisfy` \case
          Right soEnableRemoteSchemaPermissions -> soEnableRemoteSchemaPermissions == Options.EnableRemoteSchemaPermissions
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoEnableRemoteSchemaPermissions = True}
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableRemoteSchemaPermissions result `Hspec.shouldSatisfy` \case
          Right soEnableRemoteSchemaPermissions -> soEnableRemoteSchemaPermissions == Options.EnableRemoteSchemaPermissions
          Left _err -> False

    Hspec.describe "soWebSocketCompression" $ do
      Hspec.it "Default == NoCompression" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (WS.connectionCompressionOptions . UUT.soConnectionOptions) result `Hspec.shouldSatisfy` \case
          Right soWebSocketCompression -> soWebSocketCompression == WS.NoCompression
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_CONNECTION_COMPRESSION", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (WS.connectionCompressionOptions . UUT.soConnectionOptions) result `Hspec.shouldSatisfy` \case
          Right soWebSocketCompression -> soWebSocketCompression == WS.PermessageDeflateCompression WS.defaultPermessageDeflate
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoWebSocketCompression = True}
            -- When
            env = [("HASURA_GRAPHQL_CONNECTION_COMPRESSION", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (WS.connectionCompressionOptions . UUT.soConnectionOptions) result `Hspec.shouldSatisfy` \case
          Right soWebSocketCompression -> soWebSocketCompression == WS.PermessageDeflateCompression WS.defaultPermessageDeflate
          Left _err -> False

    Hspec.describe "soWebSocketKeepAlive" $ do
      Hspec.it "Default == 5" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soWebSocketKeepAlive) result `Hspec.shouldSatisfy` \case
          Right soWebSocketKeepAlive -> soWebSocketKeepAlive == UUT.KeepAliveDelay 5
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_WEBSOCKET_KEEPALIVE", "10")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soWebSocketKeepAlive) result `Hspec.shouldSatisfy` \case
          Right soWebSocketKeepAlive -> soWebSocketKeepAlive == UUT.KeepAliveDelay 10
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoWebSocketKeepAlive = Just 20}
            -- When
            env = [("HASURA_GRAPHQL_WEBSOCKET_KEEPALIVE", "10")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soWebSocketKeepAlive) result `Hspec.shouldSatisfy` \case
          Right soWebSocketKeepAlive -> soWebSocketKeepAlive == UUT.KeepAliveDelay 20
          Left _err -> False

    Hspec.describe "soInferFunctionPermissions" $ do
      Hspec.it "Default == FunctionPermissionsInferred" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soInferFunctionPermissions) result `Hspec.shouldSatisfy` \case
          Right soInferFunctionPermissions -> soInferFunctionPermissions == Options.InferFunctionPermissions
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_INFER_FUNCTION_PERMISSIONS", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soInferFunctionPermissions) result `Hspec.shouldSatisfy` \case
          Right soInferFunctionPermissions -> soInferFunctionPermissions == Options.Don'tInferFunctionPermissions
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoInferFunctionPermissions = Just True}
            -- When
            env = [("HASURA_GRAPHQL_INFER_FUNCTION_PERMISSIONS", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soInferFunctionPermissions) result `Hspec.shouldSatisfy` \case
          Right soInferFunctionPermissions -> soInferFunctionPermissions == Options.InferFunctionPermissions
          Left _err -> False

    Hspec.describe "soEnableMaintenanceMode" $ do
      Hspec.it "Defaut == MaintenanceModeDisabled" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soEnableMaintenanceMode) result `Hspec.shouldSatisfy` \case
          Right soEnableMaintenanceMode -> soEnableMaintenanceMode == Types.MaintenanceModeDisabled
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_MAINTENANCE_MODE", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soEnableMaintenanceMode) result `Hspec.shouldSatisfy` \case
          Right soEnableMaintenanceMode -> soEnableMaintenanceMode == Types.MaintenanceModeEnabled ()
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoEnableMaintenanceMode = True}
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_MAINTENANCE_MODE", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soEnableMaintenanceMode) result `Hspec.shouldSatisfy` \case
          Right soEnableMaintenanceMode -> soEnableMaintenanceMode == Types.MaintenanceModeEnabled ()
          Left _err -> False

    Hspec.describe "soSchemaPollInterval" $ do
      Hspec.it "Default == 1000" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soSchemaPollInterval) result `Hspec.shouldSatisfy` \case
          Right soSchemaPollInterval -> soSchemaPollInterval == UUT.Interval 1000
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_SCHEMA_SYNC_POLL_INTERVAL", "2000")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soSchemaPollInterval) result `Hspec.shouldSatisfy` \case
          Right soSchemaPollInterval -> soSchemaPollInterval == UUT.Interval 2000
          Left _err -> False

      Hspec.it "0 == Skip" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            env = [("HASURA_GRAPHQL_SCHEMA_SYNC_POLL_INTERVAL", "0")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soSchemaPollInterval) result `Hspec.shouldSatisfy` \case
          Right soSchemaPollInterval -> soSchemaPollInterval == UUT.Skip
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoSchemaPollInterval = Just 3000}
            -- When
            env = [("HASURA_GRAPHQL_SCHEMA_SYNC_POLL_INTERVAL", "2000")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soSchemaPollInterval) result `Hspec.shouldSatisfy` \case
          Right soSchemaPollInterval -> soSchemaPollInterval == UUT.Interval 3000
          Left _err -> False

    Hspec.describe "soExperimentalFeatures" $ do
      Hspec.it "Default == mempty" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soExperimentalFeatures) result `Hspec.shouldSatisfy` \case
          Right soExperimentalFeatures ->
            soExperimentalFeatures == mempty
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            -- When
            env = [("HASURA_GRAPHQL_EXPERIMENTAL_FEATURES", "inherited_roles,optimize_permission_filters,naming_convention")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soExperimentalFeatures) result `Hspec.shouldSatisfy` \case
          Right soExperimentalFeatures ->
            soExperimentalFeatures == Set.fromList [Types.EFInheritedRoles, Types.EFOptimizePermissionFilters, Types.EFNamingConventions]
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoExperimentalFeatures = Just [Types.EFInheritedRoles, Types.EFOptimizePermissionFilters, Types.EFNamingConventions]}
            -- When
            env = [("HASURA_GRAPHQL_EXPERIMENTAL_FEATURES", "inherited_roles")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soExperimentalFeatures) result `Hspec.shouldSatisfy` \case
          Right soExperimentalFeatures ->
            soExperimentalFeatures == Set.fromList [Types.EFInheritedRoles, Types.EFOptimizePermissionFilters, Types.EFNamingConventions]
          Left _err -> False

    Hspec.describe "soEventsFetchBatchSize" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            -- When
            env = [("HASURA_GRAPHQL_EVENTS_FETCH_BATCH_SIZE", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soEventsFetchBatchSize) result `Hspec.shouldSatisfy` \case
          Right soEventsFetchBatchSize -> soEventsFetchBatchSize == 200
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoEventsFetchBatchSize = Just 300}
            -- When
            env = [("HASURA_GRAPHQL_EVENTS_FETCH_BATCH_SIZE", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soEventsFetchBatchSize) result `Hspec.shouldSatisfy` \case
          Right soEventsFetchBatchSize -> soEventsFetchBatchSize == 300
          Left _err -> False

    Hspec.describe "soGracefulShutdownTimeout" $ do
      Hspec.it "Default == 60" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soGracefulShutdownTimeout) result `Hspec.shouldSatisfy` \case
          Right soGracefulShutdownTimeout -> soGracefulShutdownTimeout == 60
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            -- When
            env = [("HASURA_GRAPHQL_GRACEFUL_SHUTDOWN_TIMEOUT", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soGracefulShutdownTimeout) result `Hspec.shouldSatisfy` \case
          Right soGracefulShutdownTimeout -> soGracefulShutdownTimeout == 200
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoGracefulShutdownTimeout = Just 300}
            -- When
            env = [("HASURA_GRAPHQL_GRACEFUL_SHUTDOWN_TIMEOUT", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soGracefulShutdownTimeout) result `Hspec.shouldSatisfy` \case
          Right soGracefulShutdownTimeout -> soGracefulShutdownTimeout == 300
          Left _err -> False

    Hspec.describe "soWebSocketConnectionInitTimeout" $ do
      Hspec.it "Default == 3" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soWebSocketConnectionInitTimeout) result `Hspec.shouldSatisfy` \case
          Right soWebSocketConnectionInitTimeout -> soWebSocketConnectionInitTimeout == UUT.WSConnectionInitTimeout 3
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            -- When
            env = [("HASURA_GRAPHQL_WEBSOCKET_CONNECTION_INIT_TIMEOUT", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soWebSocketConnectionInitTimeout) result `Hspec.shouldSatisfy` \case
          Right soWebSocketConnectionInitTimeout -> soWebSocketConnectionInitTimeout == UUT.WSConnectionInitTimeout 200
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoWebSocketConnectionInitTimeout = Just 300}
            -- When
            env = [("HASURA_GRAPHQL_WEBSOCKET_CONNECTION_INIT_TIMEOUT", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soWebSocketConnectionInitTimeout) result `Hspec.shouldSatisfy` \case
          Right soWebSocketConnectionInitTimeout -> soWebSocketConnectionInitTimeout == UUT.WSConnectionInitTimeout 300
          Left _err -> False

    Hspec.describe "soEnableMetadataQueryLoggingEnv" $ do
      Hspec.it "Default == MetadataQueryLoggingDisabled" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soEnableMetadataQueryLogging) result `Hspec.shouldSatisfy` \case
          Right soEnableMetadataQueryLogging -> soEnableMetadataQueryLogging == Logging.MetadataQueryLoggingDisabled
          Left _err -> False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_METADATA_QUERY_LOGGING", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soEnableMetadataQueryLogging) result `Hspec.shouldSatisfy` \case
          Right soEnableMetadataQueryLogging -> soEnableMetadataQueryLogging == Logging.MetadataQueryLoggingEnabled
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoEnableMetadataQueryLoggingEnv = True}
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_METADATA_QUERY_LOGGING", "False")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soEnableMetadataQueryLogging) result `Hspec.shouldSatisfy` \case
          Right soEnableMetadataQueryLogging -> soEnableMetadataQueryLogging == Logging.MetadataQueryLoggingEnabled
          Left _err -> False

    Hspec.describe "soDefaultNamingConvention" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions
            -- When
            -- When
            env = [("HASURA_GRAPHQL_DEFAULT_NAMING_CONVENTION", "graphql-default")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soDefaultNamingConvention) result `Hspec.shouldSatisfy` \case
          Right soDefaultNamingConvention -> soDefaultNamingConvention == Just NC.GraphqlCase
          Left _err -> False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyRawServeOptions {UUT.rsoDefaultNamingConvention = Just NC.GraphqlCase}
            -- When
            env = [("HASURA_GRAPHQL_DEFAULT_NAMING_CONVENTION", "hasura-default")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soDefaultNamingConvention) result `Hspec.shouldSatisfy` \case
          Right soDefaultNamingConvention -> soDefaultNamingConvention == Just NC.GraphqlCase
          Left _err -> False
