module Hasura.Server.InitSpec
  ( spec,
  )
where

--------------------------------------------------------------------------------

import Data.HashSet qualified as Set
import Data.Monoid (All (..))
import Database.PG.Query qualified as Q
import Hasura.GraphQL.Execute.Subscription.Options (SubscriptionsOptions (_lqoBatchSize))
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

{-# ANN module ("HLint: ignore Redundant ==" :: String) #-}

--------------------------------------------------------------------------------

spec :: Hspec.Spec
spec = Hspec.describe "Init Tests" $ do
  mkServeOptionsSpec

--------------------------------------------------------------------------------

emptyServeOptionsRaw :: UUT.ServeOptionsRaw Logging.Hasura
emptyServeOptionsRaw =
  UUT.ServeOptionsRaw
    { rsoPort = Nothing,
      rsoHost = Nothing,
      rsoConnParams =
        UUT.ConnParamsRaw
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
      rsoStringifyNum = Options.Don'tStringifyNumbers,
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
      rsoEnableRemoteSchemaPermissions = Options.DisableRemoteSchemaPermissions,
      rsoWebSocketCompression = False,
      rsoWebSocketKeepAlive = Nothing,
      rsoInferFunctionPermissions = Nothing,
      rsoEnableMaintenanceMode = Types.MaintenanceModeDisabled,
      rsoSchemaPollInterval = Nothing,
      rsoExperimentalFeatures = Nothing,
      rsoEventsFetchBatchSize = Nothing,
      rsoGracefulShutdownTimeout = Nothing,
      rsoWebSocketConnectionInitTimeout = Nothing,
      rsoEnableMetadataQueryLoggingEnv = Logging.MetadataQueryLoggingDisabled,
      rsoDefaultNamingConvention = Nothing
    }

mkServeOptionsSpec :: Hspec.Spec
mkServeOptionsSpec =
  Hspec.describe "mkServeOptions" $ do
    Hspec.describe "soPort" $ do
      Hspec.it "Default == 8080" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soPort result `Hspec.shouldBe` Right 8080

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_SERVER_PORT", "420")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soPort result `Hspec.shouldBe` Right 420

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoPort = Just 11}
            -- When
            env = [("HASURA_GRAPHQL_SERVER_PORT", "420")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soPort result `Hspec.shouldBe` Right 11

    Hspec.describe "soHost" $ do
      Hspec.it "Default = '*'" $ do
        -- Given
        let rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soHost result `Hspec.shouldBe` Right "*"

      Hspec.it "Env > Nothing" $ do
        -- Given
        let rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_SERVER_HOST", "127.0.0.1")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soHost result `Hspec.shouldBe` Right "127.0.0.1"

      Hspec.it "Arg > Env" $ do
        -- Given
        let rawServeOptions = emptyServeOptionsRaw {UUT.rsoHost = Just "*4"}
            -- When
            env = [("HASURA_GRAPHQL_SERVER_HOST", "127.0.0.1")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soHost result `Hspec.shouldBe` Right "*4"

    Hspec.describe "soConnParams" $ do
      Hspec.it "Default == 1, 50, 180, 600" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soConnParams result
          `Hspec.shouldBe` Right
            ( Q.ConnParams
                { Q.cpStripes = 1,
                  Q.cpConns = 50,
                  Q.cpIdleTime = 180,
                  Q.cpAllowPrepare = True,
                  Q.cpMbLifetime = Just 600,
                  Q.cpTimeout = Nothing,
                  Q.cpCancel = True
                }
            )

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
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

        fmap UUT.soConnParams result
          `Hspec.shouldBe` Right
            ( Q.ConnParams
                { Q.cpStripes = 42,
                  Q.cpConns = 43,
                  Q.cpIdleTime = 44,
                  Q.cpAllowPrepare = False,
                  Q.cpMbLifetime = Just 45,
                  Q.cpTimeout = Just 46,
                  Q.cpCancel = True
                }
            )

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions =
              emptyServeOptionsRaw
                { UUT.rsoConnParams =
                    UUT.ConnParamsRaw
                      { rcpStripes = Just 2,
                        rcpConns = Just 3,
                        rcpIdleTime = Just 4,
                        rcpConnLifetime = Just 5,
                        rcpAllowPrepare = Just True,
                        rcpPoolTimeout = Just 6
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

        fmap UUT.soConnParams result
          `Hspec.shouldBe` Right
            ( Q.ConnParams
                { cpStripes = 2,
                  cpConns = 3,
                  cpIdleTime = 4,
                  cpAllowPrepare = True,
                  cpMbLifetime = Just 5,
                  cpTimeout = Just 6,
                  cpCancel = True
                }
            )

    Hspec.describe "soTxIso" $ do
      Hspec.it "Default == ReadCommitted" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soTxIso result `Hspec.shouldBe` Right Q.ReadCommitted

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_TX_ISOLATION", "repeatable-read")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soTxIso result `Hspec.shouldBe` Right Q.RepeatableRead

      Hspec.it "Arg > Env" $ do
        -- Given
        let rawServeOptions = emptyServeOptionsRaw {UUT.rsoTxIso = Just Q.Serializable}
            -- When
            env = [("HASURA_GRAPHQL_TX_ISOLATION", "repeatable-read")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soTxIso result `Hspec.shouldBe` Right Q.Serializable

    Hspec.describe "soAdminSecret" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_ADMIN_SECRET", "A monad is a monoid in the category of endofunctors")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soAdminSecret result `Hspec.shouldBe` Right (Set.singleton (Auth.hashAdminSecret "A monad is a monoid in the category of endofunctors"))

      Hspec.it "Arg > Env" $ do
        -- Given
        let rawServeOptions = emptyServeOptionsRaw {UUT.rsoAdminSecret = Just (Auth.hashAdminSecret "Whats the big deal")}
            -- When
            env = [("HASURA_GRAPHQL_ADMIN_SECRET", "A monad is a monoid in the category of endofunctors")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soAdminSecret result `Hspec.shouldBe` Right (Set.singleton (Auth.hashAdminSecret "Whats the big deal"))

    Hspec.describe "soAuthHook" $ do
      Hspec.it "Default Hook Mode == GET" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_AUTH_HOOK", "http://auth.hook.com")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soAuthHook result `Hspec.shouldBe` Right (Just (UUT.AuthHookG "http://auth.hook.com" UUT.AHTGet))

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env =
              [ ("HASURA_GRAPHQL_AUTH_HOOK", "http://auth.hook.com"),
                ("HASURA_GRAPHQL_AUTH_HOOK_MODE", "POST")
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soAuthHook result `Hspec.shouldBe` Right (Just (UUT.AuthHookG "http://auth.hook.com" UUT.AHTPost))

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoAuthHook = UUT.AuthHookG (Just "http://auth.hook.com") (Just UUT.AHTGet)}
            -- When
            env =
              [ ("HASURA_GRAPHQL_AUTH_HOOK", "http://auth.hook.net"),
                ("HASURA_GRAPHQL_AUTH_HOOK_MODE", "POST")
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soAuthHook result `Hspec.shouldBe` Right (Just (UUT.AuthHookG "http://auth.hook.com" UUT.AHTGet))

    Hspec.describe "soJwtSecret" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env =
              [ ( "HASURA_GRAPHQL_JWT_SECRET",
                  "{\"type\": \"HS256\", \"key\": \"11111111111111111111111111111111\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}"
                )
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soJwtSecret result
          `Hspec.shouldBe` UUT.fromEnv "[{\"type\": \"HS256\", \"key\": \"11111111111111111111111111111111\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}]"

      Hspec.it "Arg > Env" $ do
        let -- Given
            jwtConfig = eitherToMaybe $ UUT.fromEnv @UUT.JWTConfig "{\"type\": \"HS256\", \"key\": \"22222222222222222222222222222222\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}"
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoJwtSecret = jwtConfig}
            -- When
            env =
              [ ( "HASURA_GRAPHQL_JWT_SECRET",
                  "{\"type\": \"HS256\", \"key\": \"11111111111111111111111111111111\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}"
                )
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soJwtSecret result `Hspec.shouldBe` Right (onNothing jwtConfig [])

    Hspec.describe "soUnAuthRole" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_UNAUTHORIZED_ROLE", "guest")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soUnAuthRole result `Hspec.shouldBe` Right (UUT.mkRoleName "guest")

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoUnAuthRole = UUT.mkRoleName "visitor"}
            -- When
            env = [("HASURA_GRAPHQL_UNAUTHORIZED_ROLE", "guest")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soUnAuthRole result `Hspec.shouldBe` Right (UUT.mkRoleName "visitor")

    Hspec.describe "soCorsConfig" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_CORS_DOMAIN", "http://domain1:23, http://domain2:34")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soCorsConfig result `Hspec.shouldBe` Cors.readCorsDomains "http://domain1:23, http://domain2:34"

      Hspec.it "Default CorsConfig == CCAllowAll" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soCorsConfig result `Hspec.shouldBe` Right Cors.CCAllowAll

      Hspec.it "Env 'HASURA_GRAPHQL_DISABLE_CORS=false' superseded by 'HASURA_GRAPHQL_CORS_DOMAIN'" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_CORS_DOMAIN", "http://domain1:23, http://domain2:34"), ("HASURA_GRAPHQL_DISABLE_CORS", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soCorsConfig result `Hspec.shouldBe` Cors.readCorsDomains "http://domain1:23, http://domain2:34"

      Hspec.it "Env 'HASURA_GRAPHQL_DISABLE_CORS=true' enables use of cookie value" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_WS_READ_COOKIE", "true"), ("HASURA_GRAPHQL_DISABLE_CORS", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soCorsConfig result `Hspec.shouldBe` Right (Cors.CCDisabled True)

      Hspec.it "Env 'HASURA_GRAPHQL_DISABLE_CORS=true' supersedes 'HASURA_GRAPHQL_CORS_DOMAIN'" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_CORS_DOMAIN", "http://domain1:23, http://domain2:34"), ("HASURA_GRAPHQL_DISABLE_CORS", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soCorsConfig result `Hspec.shouldBe` Right (Cors.CCDisabled False)

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoCorsConfig = eitherToMaybe (Cors.readCorsDomains "http://domain1:23, http://domain2:34")}
            -- When
            env = [("HASURA_GRAPHQL_CORS_DOMAIN", "http://domain3:23, http://domain4:34")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soCorsConfig result `Hspec.shouldBe` Cors.readCorsDomains "http://domain1:23, http://domain2:34"

    Hspec.describe "soEnableConsole" $ do
      Hspec.it "Default == False" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableConsole result `Hspec.shouldBe` Right False

      -- NOTE: This is a little confusing. We are first simulating
      -- not providing the '--enable-console' flag with the env var set to 'true'.
      -- Next we simulate providing the '--enable-console' flag and
      -- the env var to ensure the flag supersedes the env var.
      Hspec.it "Env > No Switch" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_CONSOLE", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableConsole result `Hspec.shouldBe` Right True

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEnableConsole = True}
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_CONSOLE", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableConsole result `Hspec.shouldBe` Right True

    Hspec.describe "soConsoleAssetsDir" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_CONSOLE_ASSETS_DIR", "/assets")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soConsoleAssetsDir result `Hspec.shouldBe` Right (Just "/assets")

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoConsoleAssetsDir = Just "/data"}
            -- When
            env = [("HASURA_GRAPHQL_CONSOLE_ASSETS_DIR", "/assets")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soConsoleAssetsDir result `Hspec.shouldBe` Right (Just "/data")

    Hspec.describe "soEnableTelemetry" $ do
      Hspec.it "Default == True" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableTelemetry result `Hspec.shouldBe` Right True

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_TELEMETRY", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableTelemetry result `Hspec.shouldBe` Right False

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEnableTelemetry = Just False}
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_TELEMETRY", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableTelemetry result `Hspec.shouldBe` Right False

    Hspec.describe "soStringifyNum" $ do
      Hspec.it "Default == LeaveNumbersAlone" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soStringifyNum result `Hspec.shouldBe` Right Options.Don'tStringifyNumbers

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soStringifyNum result `Hspec.shouldBe` Right Options.StringifyNumbers

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoStringifyNum = Options.StringifyNumbers}
            -- When
            env = [("HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soStringifyNum result `Hspec.shouldBe` Right Options.StringifyNumbers

    Hspec.describe "soDangerousBooleanCollapse" $ do
      Hspec.it "Default == False" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soDangerousBooleanCollapse result `Hspec.shouldBe` Right False

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_V1_BOOLEAN_NULL_COLLAPSE", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soDangerousBooleanCollapse result `Hspec.shouldBe` Right True

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoDangerousBooleanCollapse = Just False}
            -- When
            env = [("HASURA_GRAPHQL_V1_BOOLEAN_NULL_COLLAPSE", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soDangerousBooleanCollapse result `Hspec.shouldBe` Right False

    Hspec.describe "soEnabledAPIs" $ do
      Hspec.it "Default == metadata,graphql,pgdump,config" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnabledAPIs result `Hspec.shouldBe` Right (Set.fromList [UUT.METADATA, UUT.GRAPHQL, UUT.PGDUMP, UUT.CONFIG])

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_ENABLED_APIS", "metadata,graphql")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnabledAPIs result `Hspec.shouldBe` Right (Set.fromList [UUT.METADATA, UUT.GRAPHQL])

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEnabledAPIs = Just [UUT.CONFIG]}
            -- When
            env = [("HASURA_GRAPHQL_ENABLED_APIS", "metadata,graphql")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnabledAPIs result `Hspec.shouldBe` Right (Set.fromList [UUT.CONFIG])

    Hspec.describe "soLiveQueryOpts" $ do
      Hspec.it "Default == 1000, 100" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soLiveQueryOpts result
          `Hspec.shouldBe` Right
            ( ESO.SubscriptionsOptions
                { _lqoRefetchInterval = ESO.RefetchInterval 1,
                  _lqoBatchSize = ESO.BatchSize 100
                }
            )

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env =
              [ ("HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_REFETCH_INTERVAL", "2000"),
                ("HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_BATCH_SIZE", "200")
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soLiveQueryOpts result
          `Hspec.shouldBe` Right
            ( ESO.SubscriptionsOptions
                { _lqoRefetchInterval = ESO.RefetchInterval 2,
                  _lqoBatchSize = ESO.BatchSize 200
                }
            )

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions =
              emptyServeOptionsRaw
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

        fmap UUT.soLiveQueryOpts result
          `Hspec.shouldBe` Right
            ( ESO.SubscriptionsOptions
                { _lqoRefetchInterval = ESO.RefetchInterval 3,
                  _lqoBatchSize = ESO.BatchSize 300
                }
            )

    Hspec.describe "soStreamingQueryOpts" $ do
      Hspec.it "Default == 1000, 100" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soStreamingQueryOpts result
          `Hspec.shouldBe` Right
            ( ESO.SubscriptionsOptions
                { _lqoRefetchInterval = ESO.RefetchInterval 1,
                  _lqoBatchSize = ESO.BatchSize 100
                }
            )

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env =
              [ ("HASURA_GRAPHQL_STREAMING_QUERIES_MULTIPLEXED_REFETCH_INTERVAL", "2000"),
                ("HASURA_GRAPHQL_STREAMING_QUERIES_MULTIPLEXED_BATCH_SIZE", "200")
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soStreamingQueryOpts result
          `Hspec.shouldBe` Right
            ( ESO.SubscriptionsOptions
                { _lqoRefetchInterval = ESO.RefetchInterval 2,
                  _lqoBatchSize = ESO.BatchSize 200
                }
            )

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions =
              emptyServeOptionsRaw
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

        fmap UUT.soStreamingQueryOpts result
          `Hspec.shouldBe` Right
            ( ESO.SubscriptionsOptions
                { _lqoRefetchInterval = ESO.RefetchInterval 3,
                  _lqoBatchSize = ESO.BatchSize 300
                }
            )

    Hspec.describe "soEnableAllowlist" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_ALLOWLIST", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableAllowlist result `Hspec.shouldBe` Right True

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEnableAllowlist = True}
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_ALLOWLIST", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableAllowlist result `Hspec.shouldBe` Right True

    Hspec.describe "soEnabledLogTypes" $ do
      Hspec.it "Default == Startup, HttpLog, WebhookLog, WebsocketLog" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnabledLogTypes result `Hspec.shouldBe` Right (Set.fromList [Logging.ELTStartup, Logging.ELTHttpLog, Logging.ELTWebhookLog, Logging.ELTWebsocketLog])

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_ENABLED_LOG_TYPES", "http-log")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnabledLogTypes result `Hspec.shouldBe` Right (Set.fromList [Logging.ELTHttpLog])

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEnabledLogTypes = Just [Logging.ELTActionHandler]}
            -- When
            env = [("HASURA_GRAPHQL_ENABLED_LOG_TYPES", "http-log")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnabledLogTypes result `Hspec.shouldBe` Right (Set.fromList [Logging.ELTActionHandler])

    Hspec.describe "soLogLevel" $ do
      Hspec.it "Default == LevelInfo" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soLogLevel result `Hspec.shouldBe` Right Logging.LevelInfo

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_LOG_LEVEL", "warn")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soLogLevel result `Hspec.shouldBe` Right Logging.LevelWarn

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoLogLevel = Just Logging.LevelWarn}
            -- When
            env = [("HASURA_GRAPHQL_LOG_LEVEL", "warn")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soLogLevel result `Hspec.shouldBe` Right Logging.LevelWarn

    Hspec.describe "soDevMode" $ do
      Hspec.it "Default == False" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
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
            rawServeOptions = emptyServeOptionsRaw
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
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoDevMode = True}
            -- When
            env = [("HASURA_GRAPHQL_DEV_MODE", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soDevMode result `Hspec.shouldBe` Right True

    Hspec.describe "soAdminInternalErrors" $ do
      Hspec.it "Default == InternalErrorsAdminOnly" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soResponseInternalErrorsConfig result `Hspec.shouldBe` Right UUT.InternalErrorsAdminOnly

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_ADMIN_INTERNAL_ERRORS", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soResponseInternalErrorsConfig result `Hspec.shouldBe` Right UUT.InternalErrorsDisabled

      Hspec.it "Dev Mode supersedes rsoAdminInternalErrors" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_ADMIN_INTERNAL_ERRORS", "false"), ("HASURA_GRAPHQL_DEV_MODE", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soResponseInternalErrorsConfig result `Hspec.shouldBe` Right UUT.InternalErrorsAllRequests

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoAdminInternalErrors = Just False}
            -- When
            env = [("HASURA_GRAPHQL_ADMIN_INTERNAL_ERRORS", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soResponseInternalErrorsConfig result `Hspec.shouldBe` Right UUT.InternalErrorsDisabled

    Hspec.describe "soEventsHttpPoolSize" $ do
      -- TODO(SOLOMON): Despite the parser help message, there
      -- actually isn't a default value set in 'ServeOptions' for
      -- this:
      --Hspec.it "Default == 100" $ do
      --  let -- Given
      --      rawServeOptions = emptyServeOptionsRaw
      --      -- When
      --      env = []
      --      -- Then
      --      result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

      --  fmap UUT.soEventsHttpPoolSize result `Hspec.shouldBe` Right (Just 100)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEventsHttpPoolSize result `Hspec.shouldBe` Right (Just 200)

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEventsHttpPoolSize = Just 300}
            -- When
            env = [("HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEventsHttpPoolSize result `Hspec.shouldBe` Right (Just 300)

    Hspec.describe "soEventsFetchInterval" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEventsFetchInterval result `Hspec.shouldBe` Right (Just 200)

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEventsFetchInterval = Just 300}
            -- When
            env = [("HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEventsFetchInterval result `Hspec.shouldBe` Right (Just 300)

    Hspec.describe "soAsyncActionsFetchInterval" $ do
      Hspec.it "Default == 1000" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soAsyncActionsFetchInterval result `Hspec.shouldBe` Right (UUT.Interval 1000)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_ASYNC_ACTIONS_FETCH_INTERVAL", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soAsyncActionsFetchInterval result `Hspec.shouldBe` Right (UUT.Interval 200)

      Hspec.it "0 == 'Skip'" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_ASYNC_ACTIONS_FETCH_INTERVAL", "0")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soAsyncActionsFetchInterval result `Hspec.shouldBe` Right UUT.Skip

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoAsyncActionsFetchInterval = Just (UUT.Interval 300)}
            -- When
            env = [("HASURA_GRAPHQL_ASYNC_ACTIONS_FETCH_INTERVAL", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soAsyncActionsFetchInterval result `Hspec.shouldBe` Right (UUT.Interval 300)

    Hspec.describe "soEnableRemoteSchemaPermissions" $ do
      Hspec.it "Default == False" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableRemoteSchemaPermissions result `Hspec.shouldBe` Right Options.DisableRemoteSchemaPermissions

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableRemoteSchemaPermissions result `Hspec.shouldBe` Right Options.EnableRemoteSchemaPermissions

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEnableRemoteSchemaPermissions = Options.EnableRemoteSchemaPermissions}
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEnableRemoteSchemaPermissions result `Hspec.shouldBe` Right Options.EnableRemoteSchemaPermissions

    Hspec.describe "soWebSocketCompression" $ do
      Hspec.it "Default == NoCompression" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (WS.connectionCompressionOptions . UUT.soConnectionOptions) result `Hspec.shouldBe` Right WS.NoCompression

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_CONNECTION_COMPRESSION", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (WS.connectionCompressionOptions . UUT.soConnectionOptions) result
          `Hspec.shouldBe` Right (WS.PermessageDeflateCompression WS.defaultPermessageDeflate)

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoWebSocketCompression = True}
            -- When
            env = [("HASURA_GRAPHQL_CONNECTION_COMPRESSION", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (WS.connectionCompressionOptions . UUT.soConnectionOptions) result
          `Hspec.shouldBe` Right (WS.PermessageDeflateCompression WS.defaultPermessageDeflate)

    Hspec.describe "soWebSocketKeepAlive" $ do
      Hspec.it "Default == 5" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soWebSocketKeepAlive) result `Hspec.shouldBe` Right (UUT.KeepAliveDelay 5)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_WEBSOCKET_KEEPALIVE", "10")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soWebSocketKeepAlive) result `Hspec.shouldBe` Right (UUT.KeepAliveDelay 10)

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoWebSocketKeepAlive = Just (UUT.KeepAliveDelay 20)}
            -- When
            env = [("HASURA_GRAPHQL_WEBSOCKET_KEEPALIVE", "10")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soWebSocketKeepAlive) result `Hspec.shouldBe` Right (UUT.KeepAliveDelay 20)

    Hspec.describe "soInferFunctionPermissions" $ do
      Hspec.it "Default == FunctionPermissionsInferred" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soInferFunctionPermissions) result `Hspec.shouldBe` Right Options.InferFunctionPermissions

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_INFER_FUNCTION_PERMISSIONS", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soInferFunctionPermissions) result `Hspec.shouldBe` Right Options.Don'tInferFunctionPermissions

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoInferFunctionPermissions = Just Options.InferFunctionPermissions}
            -- When
            env = [("HASURA_GRAPHQL_INFER_FUNCTION_PERMISSIONS", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soInferFunctionPermissions) result `Hspec.shouldBe` Right Options.InferFunctionPermissions

    Hspec.describe "soEnableMaintenanceMode" $ do
      Hspec.it "Defaut == MaintenanceModeDisabled" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soEnableMaintenanceMode) result `Hspec.shouldBe` Right Types.MaintenanceModeDisabled

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_MAINTENANCE_MODE", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soEnableMaintenanceMode) result `Hspec.shouldBe` Right (Types.MaintenanceModeEnabled ())

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEnableMaintenanceMode = Types.MaintenanceModeEnabled ()}
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_MAINTENANCE_MODE", "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soEnableMaintenanceMode) result `Hspec.shouldBe` Right (Types.MaintenanceModeEnabled ())

    Hspec.describe "soSchemaPollInterval" $ do
      Hspec.it "Default == 1000" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soSchemaPollInterval) result `Hspec.shouldBe` Right (UUT.Interval 1000)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_SCHEMA_SYNC_POLL_INTERVAL", "2000")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soSchemaPollInterval) result `Hspec.shouldBe` Right (UUT.Interval 2000)

      Hspec.it "0 == Skip" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_SCHEMA_SYNC_POLL_INTERVAL", "0")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soSchemaPollInterval) result `Hspec.shouldBe` Right UUT.Skip

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoSchemaPollInterval = Just (UUT.Interval 3000)}
            -- When
            env = [("HASURA_GRAPHQL_SCHEMA_SYNC_POLL_INTERVAL", "2000")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soSchemaPollInterval) result `Hspec.shouldBe` Right (UUT.Interval 3000)

    Hspec.describe "soExperimentalFeatures" $ do
      Hspec.it "Default == mempty" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soExperimentalFeatures) result `Hspec.shouldBe` Right mempty

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            -- When
            env = [("HASURA_GRAPHQL_EXPERIMENTAL_FEATURES", "inherited_roles,optimize_permission_filters,naming_convention")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soExperimentalFeatures) result
          `Hspec.shouldBe` Right (Set.fromList [Types.EFInheritedRoles, Types.EFOptimizePermissionFilters, Types.EFNamingConventions])

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoExperimentalFeatures = Just [Types.EFInheritedRoles, Types.EFOptimizePermissionFilters, Types.EFNamingConventions]}
            -- When
            env = [("HASURA_GRAPHQL_EXPERIMENTAL_FEATURES", "inherited_roles")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soExperimentalFeatures) result
          `Hspec.shouldBe` Right (Set.fromList [Types.EFInheritedRoles, Types.EFOptimizePermissionFilters, Types.EFNamingConventions])

    Hspec.describe "soEventsFetchBatchSize" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_EVENTS_FETCH_BATCH_SIZE", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soEventsFetchBatchSize) result `Hspec.shouldBe` Right 200

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEventsFetchBatchSize = Just 300}
            -- When
            env = [("HASURA_GRAPHQL_EVENTS_FETCH_BATCH_SIZE", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soEventsFetchBatchSize) result `Hspec.shouldBe` Right 300

    Hspec.describe "soGracefulShutdownTimeout" $ do
      Hspec.it "Default == 60" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soGracefulShutdownTimeout) result `Hspec.shouldBe` Right 60

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            -- When
            env = [("HASURA_GRAPHQL_GRACEFUL_SHUTDOWN_TIMEOUT", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soGracefulShutdownTimeout) result `Hspec.shouldBe` Right 200

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoGracefulShutdownTimeout = Just 300}
            -- When
            env = [("HASURA_GRAPHQL_GRACEFUL_SHUTDOWN_TIMEOUT", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soGracefulShutdownTimeout) result `Hspec.shouldBe` Right 300

    Hspec.describe "soWebSocketConnectionInitTimeout" $ do
      Hspec.it "Default == 3" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soWebSocketConnectionInitTimeout) result `Hspec.shouldBe` Right (UUT.WSConnectionInitTimeout 3)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [("HASURA_GRAPHQL_WEBSOCKET_CONNECTION_INIT_TIMEOUT", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soWebSocketConnectionInitTimeout) result `Hspec.shouldBe` Right (UUT.WSConnectionInitTimeout 200)

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoWebSocketConnectionInitTimeout = Just (UUT.WSConnectionInitTimeout 300)}
            -- When
            env = [("HASURA_GRAPHQL_WEBSOCKET_CONNECTION_INIT_TIMEOUT", "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soWebSocketConnectionInitTimeout) result `Hspec.shouldBe` Right (UUT.WSConnectionInitTimeout 300)

    Hspec.describe "soEnableMetadataQueryLoggingEnv" $ do
      Hspec.it "Default == MetadataQueryLoggingDisabled" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soEnableMetadataQueryLogging) result `Hspec.shouldBe` Right Logging.MetadataQueryLoggingDisabled

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_METADATA_QUERY_LOGGING", "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soEnableMetadataQueryLogging) result `Hspec.shouldBe` Right Logging.MetadataQueryLoggingEnabled

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEnableMetadataQueryLoggingEnv = Logging.MetadataQueryLoggingEnabled}
            -- When
            env = [("HASURA_GRAPHQL_ENABLE_METADATA_QUERY_LOGGING", "False")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soEnableMetadataQueryLogging) result `Hspec.shouldBe` Right Logging.MetadataQueryLoggingEnabled

    Hspec.describe "soDefaultNamingConvention" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            -- When
            env = [("HASURA_GRAPHQL_DEFAULT_NAMING_CONVENTION", "graphql-default")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soDefaultNamingConvention) result `Hspec.shouldBe` Right (Just NC.GraphqlCase)

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoDefaultNamingConvention = Just NC.GraphqlCase}
            -- When
            env = [("HASURA_GRAPHQL_DEFAULT_NAMING_CONVENTION", "hasura-default")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap (UUT.soDefaultNamingConvention) result `Hspec.shouldBe` Right (Just NC.GraphqlCase)
