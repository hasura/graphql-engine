{-# LANGUAGE TemplateHaskell #-}

module Hasura.Server.InitSpec
  ( spec,
  )
where

--------------------------------------------------------------------------------

import Data.HashSet qualified as Set
import Data.Time (NominalDiffTime)
import Database.PG.Query qualified as Query
import Hasura.GraphQL.Execute.Subscription.Options qualified as Subscription.Options
import Hasura.Logging (Hasura)
import Hasura.Logging qualified as Logging
import Hasura.Prelude
import Hasura.RQL.Types.NamingCase qualified as NamingCase
import Hasura.RQL.Types.Roles qualified as Roles
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.SQL.Types qualified as MonadTx
import Hasura.Server.Auth qualified as Auth
import Hasura.Server.Cors qualified as Cors
import Hasura.Server.Init qualified as UUT
import Hasura.Server.Logging qualified as Logging
import Hasura.Server.Types qualified as Types
import Network.WebSockets qualified as WS
import Refined (NonNegative, Positive, refineTH, unrefine)
import Test.Hspec qualified as Hspec

{-# ANN module ("HLint: ignore Redundant ==" :: String) #-}

--------------------------------------------------------------------------------

spec :: Hspec.Spec
spec = Hspec.describe "Init Tests" $ do
  mkServeOptionsSpec

--------------------------------------------------------------------------------

emptyServeOptionsRaw :: UUT.ServeOptionsRaw Hasura
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
      rsoAuthHook = UUT.AuthHookRaw Nothing Nothing Nothing,
      rsoJwtSecret = Nothing,
      rsoUnAuthRole = Nothing,
      rsoCorsConfig = Nothing,
      rsoConsoleStatus = UUT.ConsoleDisabled,
      rsoConsoleAssetsDir = Nothing,
      rsoConsoleSentryDsn = Nothing,
      rsoEnableTelemetry = Nothing,
      rsoWsReadCookie = UUT.WsReadCookieDisabled,
      rsoStringifyNum = Options.Don'tStringifyNumbers,
      rsoDangerousBooleanCollapse = Nothing,
      rsoRemoteNullForwardingPolicy = Nothing,
      rsoEnabledAPIs = Nothing,
      rsoMxRefetchInt = Nothing,
      rsoMxBatchSize = Nothing,
      rsoStreamingMxRefetchInt = Nothing,
      rsoStreamingMxBatchSize = Nothing,
      rsoEnableAllowList = UUT.AllowListDisabled,
      rsoEnabledLogTypes = Nothing,
      rsoLogLevel = Nothing,
      rsoDevMode = UUT.DevModeDisabled,
      rsoAdminInternalErrors = Nothing,
      rsoEventsHttpPoolSize = Nothing,
      rsoEventsFetchInterval = Nothing,
      rsoAsyncActionsFetchInterval = Nothing,
      rsoEnableRemoteSchemaPermissions = Options.DisableRemoteSchemaPermissions,
      rsoWebSocketCompression = WS.NoCompression,
      rsoWebSocketKeepAlive = Nothing,
      rsoInferFunctionPermissions = Nothing,
      rsoEnableMaintenanceMode = Types.MaintenanceModeDisabled,
      rsoSchemaPollInterval = Nothing,
      rsoExperimentalFeatures = Nothing,
      rsoEventsFetchBatchSize = Nothing,
      rsoGracefulShutdownTimeout = Nothing,
      rsoWebSocketConnectionInitTimeout = Nothing,
      rsoEnableMetadataQueryLoggingEnv = Logging.MetadataQueryLoggingDisabled,
      rsoDefaultNamingConvention = Nothing,
      rsoExtensionsSchema = Nothing,
      rsoMetadataDefaults = Nothing,
      rsoApolloFederationStatus = Nothing,
      rsoCloseWebsocketsOnMetadataChangeStatus = Nothing,
      rsoMaxTotalHeaderLength = Nothing,
      rsoTriggersErrorLogLevelStatus = Nothing,
      rsoAsyncActionsFetchBatchSize = Nothing,
      rsoPersistedQueries = Nothing,
      rsoPersistedQueriesTtl = Nothing
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
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soPort result `Hspec.shouldBe` Right (UUT._default UUT.servePortOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.servePortOption, "420")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soPort result `Hspec.shouldBe` Right (UUT.unsafePort 420)

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoPort = Just (UUT.unsafePort 11)}
            -- When
            env = [(UUT._envVar UUT.servePortOption, "420")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soPort result `Hspec.shouldBe` Right (UUT.unsafePort 11)

    Hspec.describe "soHost" $ do
      Hspec.it "Default = '*'" $ do
        -- Given
        let rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soHost result `Hspec.shouldBe` Right (UUT._default UUT.serveHostOption)

      Hspec.it "Env > Nothing" $ do
        -- Given
        let rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.serveHostOption, "127.0.0.1")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soHost result `Hspec.shouldBe` Right "127.0.0.1"

      Hspec.it "Arg > Env" $ do
        -- Given
        let rawServeOptions = emptyServeOptionsRaw {UUT.rsoHost = Just "*4"}
            -- When
            env = [(UUT._envVar UUT.serveHostOption, "127.0.0.1")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soHost result `Hspec.shouldBe` Right "*4"

    Hspec.describe "soConnParams" $ do
      Hspec.it "Default == 1, 50, 180, 600" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soConnParams result
          `Hspec.shouldBe` Right
            ( Query.ConnParams
                { Query.cpStripes = unrefine $ UUT._default UUT.pgStripesOption,
                  Query.cpConns = unrefine $ UUT._default UUT.pgConnsOption,
                  Query.cpIdleTime = unrefine $ UUT._default UUT.pgTimeoutOption,
                  Query.cpAllowPrepare = UUT._default UUT.pgUsePreparedStatementsOption,
                  Query.cpMbLifetime = Just $ unrefine $ UUT._default UUT.pgConnLifetimeOption,
                  Query.cpTimeout = Nothing,
                  Query.cpCancel = True
                }
            )

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env =
              [ (UUT._envVar UUT.pgStripesOption, "42"),
                (UUT._envVar UUT.pgConnsOption, "43"),
                (UUT._envVar UUT.pgTimeoutOption, "44"),
                (UUT._envVar UUT.pgConnLifetimeOption, "45"),
                (UUT._envVar UUT.pgUsePreparedStatementsOption, "false"),
                (UUT._envVar UUT.pgPoolTimeoutOption, "46")
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soConnParams result
          `Hspec.shouldBe` Right
            ( Query.ConnParams
                { Query.cpStripes = 42,
                  Query.cpConns = 43,
                  Query.cpIdleTime = 44,
                  Query.cpAllowPrepare = False,
                  Query.cpMbLifetime = Just 45,
                  Query.cpTimeout = Just 46,
                  Query.cpCancel = True
                }
            )

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions =
              emptyServeOptionsRaw
                { UUT.rsoConnParams =
                    UUT.ConnParamsRaw
                      { rcpStripes = Just $$(refineTH @NonNegative @Int 2),
                        rcpConns = Just $$(refineTH @NonNegative @Int 3),
                        rcpIdleTime = Just $$(refineTH @NonNegative @Int 4),
                        rcpConnLifetime = Just $$(refineTH @NonNegative @NominalDiffTime 5),
                        rcpAllowPrepare = Just True,
                        rcpPoolTimeout = Just $$(refineTH @NonNegative @NominalDiffTime 6)
                      }
                }
            -- When
            env =
              [ (UUT._envVar UUT.pgStripesOption, "42"),
                (UUT._envVar UUT.pgConnsOption, "43"),
                (UUT._envVar UUT.pgTimeoutOption, "44"),
                (UUT._envVar UUT.pgConnLifetimeOption, "45"),
                (UUT._envVar UUT.pgUsePreparedStatementsOption, "false"),
                (UUT._envVar UUT.pgPoolTimeoutOption, "46")
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soConnParams result
          `Hspec.shouldBe` Right
            ( Query.ConnParams
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
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soTxIso result `Hspec.shouldBe` Right (UUT._default UUT.txIsolationOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.txIsolationOption, "repeatable-read")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soTxIso result `Hspec.shouldBe` Right Query.RepeatableRead

      Hspec.it "Arg > Env" $ do
        -- Given
        let rawServeOptions = emptyServeOptionsRaw {UUT.rsoTxIso = Just Query.Serializable}
            -- When
            env = [(UUT._envVar UUT.txIsolationOption, "repeatable-read")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soTxIso result `Hspec.shouldBe` Right Query.Serializable

    Hspec.describe "soAdminSecret" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.adminSecretOption, "A monad is a monoid in the category of endofunctors")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soAdminSecret result `Hspec.shouldBe` Right (Set.singleton (Auth.hashAdminSecret "A monad is a monoid in the category of endofunctors"))

      Hspec.it "Arg > Env" $ do
        -- Given
        let rawServeOptions = emptyServeOptionsRaw {UUT.rsoAdminSecret = Just (Auth.hashAdminSecret "Whats the big deal")}
            -- When
            env = [(UUT._envVar UUT.adminSecretOption, "A monad is a monoid in the category of endofunctors")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soAdminSecret result `Hspec.shouldBe` Right (Set.singleton (Auth.hashAdminSecret "Whats the big deal"))

    Hspec.describe "soAuthHook" $ do
      Hspec.it "Default Hook Mode == GET" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.authHookOption, "http://auth.hook.com")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soAuthHook result `Hspec.shouldBe` Right (Just (Auth.AuthHook "http://auth.hook.com" Auth.AHTGet False))

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env =
              [ (UUT._envVar UUT.authHookOption, "http://auth.hook.com"),
                (UUT._envVar UUT.authHookModeOption, "POST")
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soAuthHook result `Hspec.shouldBe` Right (Just (Auth.AuthHook "http://auth.hook.com" Auth.AHTPost True))

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoAuthHook = UUT.AuthHookRaw (Just "http://auth.hook.com") (Just Auth.AHTGet) Nothing}
            -- When
            env =
              [ (UUT._envVar UUT.authHookOption, "http://auth.hook.com"),
                (UUT._envVar UUT.authHookModeOption, "POST")
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soAuthHook result `Hspec.shouldBe` Right (Just (Auth.AuthHook "http://auth.hook.com" Auth.AHTGet False))

    Hspec.describe "soJwtSecret" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env =
              [ ( UUT._envVar UUT.jwtSecretOption,
                  "{\"type\": \"HS256\", \"key\": \"11111111111111111111111111111111\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}"
                )
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soJwtSecret result
          `Hspec.shouldBe` UUT.fromEnv "[{\"type\": \"HS256\", \"key\": \"11111111111111111111111111111111\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}]"

      Hspec.it "Arg > Env" $ do
        let -- Given
            jwtConfig = eitherToMaybe $ UUT.fromEnv @Auth.JWTConfig "{\"type\": \"HS256\", \"key\": \"22222222222222222222222222222222\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}"
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoJwtSecret = jwtConfig}
            -- When
            env =
              [ ( UUT._envVar UUT.jwtSecretOption,
                  "{\"type\": \"HS256\", \"key\": \"11111111111111111111111111111111\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}"
                )
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soJwtSecret result `Hspec.shouldBe` Right (onNothing jwtConfig [])

    Hspec.describe "soUnAuthRole" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.unAuthRoleOption, "guest")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soUnAuthRole result `Hspec.shouldBe` Right (Roles.mkRoleName "guest")

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoUnAuthRole = Roles.mkRoleName "visitor"}
            -- When
            env = [(UUT._envVar UUT.unAuthRoleOption, "guest")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soUnAuthRole result `Hspec.shouldBe` Right (Roles.mkRoleName "visitor")

    Hspec.describe "soCorsConfig" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.corsDomainOption, "http://domain1:23, http://domain2:34")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soCorsConfig result `Hspec.shouldBe` Cors.readCorsDomains "http://domain1:23, http://domain2:34"

      Hspec.it "Default CorsConfig == CCAllowAll" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soCorsConfig result `Hspec.shouldBe` Right (UUT._default UUT.corsDomainOption)

      Hspec.it "Env 'HASURA_GRAPHQL_DISABLE_CORS=false' superseded by 'HASURA_GRAPHQL_CORS_DOMAIN'" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.corsDomainOption, "http://domain1:23, http://domain2:34"), (UUT._envVar UUT.disableCorsOption, "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soCorsConfig result `Hspec.shouldBe` Cors.readCorsDomains "http://domain1:23, http://domain2:34"

      Hspec.it "Env 'HASURA_GRAPHQL_DISABLE_CORS=true' enables use of cookie value" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.wsReadCookieOption, "true"), (UUT._envVar UUT.disableCorsOption, "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soCorsConfig result `Hspec.shouldBe` Right (Cors.CCDisabled True)

      Hspec.it "Env 'HASURA_GRAPHQL_DISABLE_CORS=true' supersedes 'HASURA_GRAPHQL_CORS_DOMAIN'" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.corsDomainOption, "http://domain1:23, http://domain2:34"), (UUT._envVar UUT.disableCorsOption, "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soCorsConfig result `Hspec.shouldBe` Right (Cors.CCDisabled False)

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoCorsConfig = eitherToMaybe (Cors.readCorsDomains "http://domain1:23, http://domain2:34")}
            -- When
            env = [(UUT._envVar UUT.corsDomainOption, "http://domain1:23, http://domain2:34")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soCorsConfig result `Hspec.shouldBe` Cors.readCorsDomains "http://domain1:23, http://domain2:34"

    Hspec.describe "soConsoleStatus" $ do
      Hspec.it "Default == False" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soConsoleStatus result `Hspec.shouldBe` Right (UUT._default UUT.enableConsoleOption)

      -- NOTE: This is a little confusing. We are first simulating
      -- not providing the '--enable-console' flag with the env var set to 'true'.
      -- Next we simulate providing the '--enable-console' flag and
      -- the env var to ensure the flag supersedes the env var.
      Hspec.it "Env > No Switch" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.enableConsoleOption, "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soConsoleStatus result `Hspec.shouldBe` Right UUT.ConsoleEnabled

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoConsoleStatus = UUT.ConsoleEnabled}
            -- When
            env = [(UUT._envVar UUT.enableConsoleOption, "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soConsoleStatus result `Hspec.shouldBe` Right UUT.ConsoleEnabled

    Hspec.describe "soConsoleAssetsDir" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.consoleAssetsDirOption, "/assets")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soConsoleAssetsDir result `Hspec.shouldBe` Right (Just "/assets")

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoConsoleAssetsDir = Just "/data"}
            -- When
            env = [("HASURA_GRAPHQL_CONSOLE_ASSETS_DIR", "/assets")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soConsoleAssetsDir result `Hspec.shouldBe` Right (Just "/data")

    Hspec.describe "soConsoleSentryDsn" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.consoleSentryDsnOption, "123123")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soConsoleSentryDsn result `Hspec.shouldBe` Right (Just "123123")

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoConsoleSentryDsn = Just "456456"}
            -- When
            env = [("HASURA_CONSOLE_SENTRY_DSN", "123123")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soConsoleSentryDsn result `Hspec.shouldBe` Right (Just "456456")

    Hspec.describe "soEnableTelemetry" $ do
      Hspec.it "Default == True" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soEnableTelemetry result `Hspec.shouldBe` Right (UUT._default UUT.enableTelemetryOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.enableTelemetryOption, "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soEnableTelemetry result `Hspec.shouldBe` Right UUT.TelemetryDisabled

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEnableTelemetry = Just UUT.TelemetryDisabled}
            -- When
            env = [(UUT._envVar UUT.enableTelemetryOption, "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soEnableTelemetry result `Hspec.shouldBe` Right UUT.TelemetryDisabled

    Hspec.describe "soStringifyNum" $ do
      Hspec.it "Default == Don'tStringifyNumbers" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soStringifyNum result `Hspec.shouldBe` Right (UUT._default UUT.stringifyNumOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.stringifyNumOption, "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soStringifyNum result `Hspec.shouldBe` Right Options.StringifyNumbers

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoStringifyNum = Options.StringifyNumbers}
            -- When
            env = [(UUT._envVar UUT.stringifyNumOption, "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soStringifyNum result `Hspec.shouldBe` Right Options.StringifyNumbers

    Hspec.describe "soDangerousBooleanCollapse" $ do
      Hspec.it "Default == False" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soDangerousBooleanCollapse result `Hspec.shouldBe` Right (UUT._default UUT.dangerousBooleanCollapseOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.dangerousBooleanCollapseOption, "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soDangerousBooleanCollapse result `Hspec.shouldBe` Right Options.DangerouslyCollapseBooleans

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoDangerousBooleanCollapse = Just Options.Don'tDangerouslyCollapseBooleans}
            -- When
            env = [(UUT._envVar UUT.dangerousBooleanCollapseOption, "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soDangerousBooleanCollapse result `Hspec.shouldBe` Right Options.Don'tDangerouslyCollapseBooleans

    Hspec.describe "soEnabledAPIs" $ do
      Hspec.it "Default == metadata,graphql,pgdump,config" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soEnabledAPIs result `Hspec.shouldBe` Right (UUT._default UUT.enabledAPIsOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.enabledAPIsOption, "metadata,graphql")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soEnabledAPIs result `Hspec.shouldBe` Right (Set.fromList [UUT.METADATA, UUT.GRAPHQL])

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEnabledAPIs = Just $ Set.fromList [UUT.CONFIG]}
            -- When
            env = [(UUT._envVar UUT.enabledAPIsOption, "metadata,graphql")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soEnabledAPIs result `Hspec.shouldBe` Right (Set.fromList [UUT.CONFIG])

    Hspec.describe "soLiveQueryOpts" $ do
      Hspec.it "Default == 1000, 100" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soLiveQueryOpts result
          `Hspec.shouldBe` Right
            ( Subscription.Options.SubscriptionsOptions
                { _lqoRefetchInterval = UUT._default UUT.mxRefetchDelayOption,
                  _lqoBatchSize = UUT._default UUT.mxBatchSizeOption
                }
            )

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env =
              [ (UUT._envVar UUT.mxRefetchDelayOption, "2000"),
                (UUT._envVar UUT.mxBatchSizeOption, "200")
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soLiveQueryOpts result
          `Hspec.shouldBe` Right
            ( Subscription.Options.SubscriptionsOptions
                { _lqoRefetchInterval = Subscription.Options.RefetchInterval $$(refineTH 2),
                  _lqoBatchSize = Subscription.Options.BatchSize $$(refineTH 200)
                }
            )

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions =
              emptyServeOptionsRaw
                { UUT.rsoMxRefetchInt = Subscription.Options.mkRefetchInterval 3,
                  UUT.rsoMxBatchSize = Subscription.Options.mkBatchSize 300
                }
            -- When
            env =
              [ (UUT._envVar UUT.mxRefetchDelayOption, "2000"),
                (UUT._envVar UUT.mxBatchSizeOption, "200")
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soLiveQueryOpts result
          `Hspec.shouldBe` Right
            ( Subscription.Options.SubscriptionsOptions
                { _lqoRefetchInterval = Subscription.Options.RefetchInterval $$(refineTH 3),
                  _lqoBatchSize = Subscription.Options.BatchSize $$(refineTH 300)
                }
            )

    Hspec.describe "soStreamingQueryOpts" $ do
      Hspec.it "Default == 1000, 100" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soStreamingQueryOpts result
          `Hspec.shouldBe` Right
            ( Subscription.Options.SubscriptionsOptions
                { _lqoRefetchInterval = UUT._default UUT.streamingMxRefetchDelayOption,
                  _lqoBatchSize = UUT._default UUT.streamingMxBatchSizeOption
                }
            )

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env =
              [ (UUT._envVar UUT.streamingMxRefetchDelayOption, "2000"),
                (UUT._envVar UUT.streamingMxBatchSizeOption, "200")
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soStreamingQueryOpts result
          `Hspec.shouldBe` Right
            ( Subscription.Options.SubscriptionsOptions
                { _lqoRefetchInterval = Subscription.Options.RefetchInterval $$(refineTH 2),
                  _lqoBatchSize = Subscription.Options.BatchSize $$(refineTH 200)
                }
            )

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions =
              emptyServeOptionsRaw
                { UUT.rsoStreamingMxRefetchInt = Subscription.Options.mkRefetchInterval 3,
                  UUT.rsoStreamingMxBatchSize = Subscription.Options.mkBatchSize 300
                }
            -- When
            env =
              [ (UUT._envVar UUT.streamingMxRefetchDelayOption, "2000"),
                (UUT._envVar UUT.streamingMxBatchSizeOption, "200")
              ]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soStreamingQueryOpts result
          `Hspec.shouldBe` Right
            ( Subscription.Options.SubscriptionsOptions
                { _lqoRefetchInterval = Subscription.Options.RefetchInterval $$(refineTH 3),
                  _lqoBatchSize = Subscription.Options.BatchSize $$(refineTH 300)
                }
            )

    Hspec.describe "soEnableAllowList" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.enableAllowlistOption, "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soEnableAllowList result `Hspec.shouldBe` Right UUT.AllowListEnabled

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEnableAllowList = UUT.AllowListEnabled}
            -- When
            env = [(UUT._envVar UUT.enableAllowlistOption, "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soEnableAllowList result `Hspec.shouldBe` Right UUT.AllowListEnabled

    Hspec.describe "soEnabledLogTypes" $ do
      Hspec.it "Default == Startup, HttpLog, WebhookLog, WebsocketLog" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soEnabledLogTypes result `Hspec.shouldBe` Right (UUT._default UUT.enabledLogsOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar (UUT.enabledLogsOption @Hasura), "http-log")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soEnabledLogTypes result `Hspec.shouldBe` Right (Set.fromList [Logging.ELTHttpLog])

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEnabledLogTypes = Just (Set.fromList [Logging.ELTActionHandler])}
            -- When
            env = [(UUT._envVar (UUT.enabledLogsOption @Hasura), "http-log")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soEnabledLogTypes result `Hspec.shouldBe` Right (Set.fromList [Logging.ELTActionHandler])

    Hspec.describe "soLogLevel" $ do
      Hspec.it "Default == LevelInfo" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soLogLevel result `Hspec.shouldBe` Right (UUT._default UUT.logLevelOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.logLevelOption, "warn")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soLogLevel result `Hspec.shouldBe` Right Logging.LevelWarn

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoLogLevel = Just Logging.LevelWarn}
            -- When
            env = [(UUT._envVar UUT.logLevelOption, "warn")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soLogLevel result `Hspec.shouldBe` Right Logging.LevelWarn

    Hspec.describe "soDevMode" $ do
      Hspec.it "Default == False" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soDevMode result `Hspec.shouldBe` Right (UUT._default UUT.graphqlDevModeOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.graphqlDevModeOption, "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soDevMode result `Hspec.shouldBe` Right UUT.DevModeEnabled

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoDevMode = UUT.DevModeEnabled}
            -- When
            env = [(UUT._envVar UUT.graphqlDevModeOption, "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soDevMode result `Hspec.shouldBe` Right UUT.DevModeEnabled

    Hspec.describe "soAdminInternalErrors" $ do
      Hspec.it "Default == InternalErrorsAdminOnly" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soAdminInternalErrors result `Hspec.shouldBe` Right (UUT._default UUT.graphqlAdminInternalErrorsOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.graphqlAdminInternalErrorsOption, "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soAdminInternalErrors result `Hspec.shouldBe` Right UUT.AdminInternalErrorsDisabled

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoAdminInternalErrors = Just UUT.AdminInternalErrorsDisabled}
            -- When
            env = [(UUT._envVar UUT.graphqlAdminInternalErrorsOption, "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soAdminInternalErrors result `Hspec.shouldBe` Right UUT.AdminInternalErrorsDisabled

    Hspec.describe "soEventsHttpPoolSize" $ do
      Hspec.it "Default == 100" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Logging.Hasura rawServeOptions)

        fmap UUT.soEventsHttpPoolSize result `Hspec.shouldBe` Right (UUT._default UUT.graphqlEventsHttpPoolSizeOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.graphqlEventsHttpPoolSizeOption, "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soEventsHttpPoolSize result `Hspec.shouldBe` Right $$(refineTH @Positive @Int 200)

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEventsHttpPoolSize = Just $$(refineTH @Positive @Int 300)}
            -- When
            env = [(UUT._envVar UUT.graphqlEventsHttpPoolSizeOption, "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soEventsHttpPoolSize result `Hspec.shouldBe` Right $$(refineTH @Positive @Int 300)

    Hspec.describe "soEventsFetchInterval" $ do
      Hspec.it "Default == 1" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soEventsFetchInterval result `Hspec.shouldBe` Right (UUT._default UUT.graphqlEventsFetchIntervalOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.graphqlEventsFetchIntervalOption, "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soEventsFetchInterval result `Hspec.shouldBe` Right $$(refineTH @NonNegative @Milliseconds 200)

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEventsFetchInterval = Just $$(refineTH @NonNegative @Milliseconds 300)}
            -- When
            env = [(UUT._envVar UUT.graphqlEventsFetchIntervalOption, "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soEventsFetchInterval result `Hspec.shouldBe` Right $$(refineTH @NonNegative @Milliseconds 300)

    Hspec.describe "soAsyncActionsFetchInterval" $ do
      Hspec.it "Default == 1000" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soAsyncActionsFetchInterval result `Hspec.shouldBe` Right (UUT._default UUT.asyncActionsFetchIntervalOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.asyncActionsFetchIntervalOption, "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soAsyncActionsFetchInterval result `Hspec.shouldBe` Right (UUT.Interval $$(refineTH 200))

      Hspec.it "0 == 'Skip'" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.asyncActionsFetchIntervalOption, "0")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soAsyncActionsFetchInterval result `Hspec.shouldBe` Right UUT.Skip

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoAsyncActionsFetchInterval = Just (UUT.Interval $$(refineTH 300))}
            -- When
            env = [(UUT._envVar UUT.asyncActionsFetchIntervalOption, "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soAsyncActionsFetchInterval result `Hspec.shouldBe` Right (UUT.Interval $$(refineTH 300))

    Hspec.describe "soEnableRemoteSchemaPermissions" $ do
      Hspec.it "Default == False" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soEnableRemoteSchemaPermissions result `Hspec.shouldBe` Right (UUT._default UUT.enableRemoteSchemaPermsOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.enableRemoteSchemaPermsOption, "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soEnableRemoteSchemaPermissions result `Hspec.shouldBe` Right Options.EnableRemoteSchemaPermissions

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEnableRemoteSchemaPermissions = Options.EnableRemoteSchemaPermissions}
            -- When
            env = [(UUT._envVar UUT.enableRemoteSchemaPermsOption, "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soEnableRemoteSchemaPermissions result `Hspec.shouldBe` Right Options.EnableRemoteSchemaPermissions

    Hspec.describe "soWebSocketCompression" $ do
      Hspec.it "Default == NoCompression" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (WS.connectionCompressionOptions . UUT.soConnectionOptions) result `Hspec.shouldBe` Right WS.NoCompression

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.webSocketCompressionOption, "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (WS.connectionCompressionOptions . UUT.soConnectionOptions) result
          `Hspec.shouldBe` Right (WS.PermessageDeflateCompression WS.defaultPermessageDeflate)

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoWebSocketCompression = (WS.PermessageDeflateCompression WS.defaultPermessageDeflate)}
            -- When
            env = [(UUT._envVar UUT.webSocketCompressionOption, "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (WS.connectionCompressionOptions . UUT.soConnectionOptions) result
          `Hspec.shouldBe` Right (WS.PermessageDeflateCompression WS.defaultPermessageDeflate)

    Hspec.describe "soWebSocketKeepAlive" $ do
      Hspec.it "Default == 5" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soWebSocketKeepAlive) result `Hspec.shouldBe` Right (UUT._default UUT.webSocketKeepAliveOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.webSocketKeepAliveOption, "10")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soWebSocketKeepAlive) result `Hspec.shouldBe` Right (UUT.KeepAliveDelay $$(refineTH 10))

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoWebSocketKeepAlive = Just (UUT.KeepAliveDelay $$(refineTH 20))}
            -- When
            env = [(UUT._envVar UUT.webSocketKeepAliveOption, "10")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soWebSocketKeepAlive) result `Hspec.shouldBe` Right (UUT.KeepAliveDelay $$(refineTH 20))

    Hspec.describe "soInferFunctionPermissions" $ do
      Hspec.it "Default == FunctionPermissionsInferred" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soInferFunctionPermissions) result `Hspec.shouldBe` Right (UUT._default UUT.inferFunctionPermsOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.inferFunctionPermsOption, "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soInferFunctionPermissions) result `Hspec.shouldBe` Right Options.Don'tInferFunctionPermissions

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoInferFunctionPermissions = Just Options.InferFunctionPermissions}
            -- When
            env = [(UUT._envVar UUT.inferFunctionPermsOption, "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soInferFunctionPermissions) result `Hspec.shouldBe` Right Options.InferFunctionPermissions

    Hspec.describe "soEnableMaintenanceMode" $ do
      Hspec.it "Defaut == MaintenanceModeDisabled" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soEnableMaintenanceMode) result `Hspec.shouldBe` Right (UUT._default UUT.enableMaintenanceModeOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.enableMaintenanceModeOption, "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soEnableMaintenanceMode) result `Hspec.shouldBe` Right (Types.MaintenanceModeEnabled ())

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEnableMaintenanceMode = Types.MaintenanceModeEnabled ()}
            -- When
            env = [(UUT._envVar UUT.enableMaintenanceModeOption, "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soEnableMaintenanceMode) result `Hspec.shouldBe` Right (Types.MaintenanceModeEnabled ())

    Hspec.describe "soSchemaPollInterval" $ do
      Hspec.it "Default == 1000" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soSchemaPollInterval) result `Hspec.shouldBe` Right (UUT._default UUT.schemaPollIntervalOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.schemaPollIntervalOption, "2000")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soSchemaPollInterval) result `Hspec.shouldBe` Right (UUT.Interval $$(refineTH 2000))

      Hspec.it "0 == Skip" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.schemaPollIntervalOption, "0")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soSchemaPollInterval) result `Hspec.shouldBe` Right UUT.Skip

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoSchemaPollInterval = Just (UUT.Interval $$(refineTH 3000))}
            -- When
            env = [(UUT._envVar UUT.schemaPollIntervalOption, "2000")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soSchemaPollInterval) result `Hspec.shouldBe` Right (UUT.Interval $$(refineTH 3000))

    Hspec.describe "soExperimentalFeatures" $ do
      Hspec.it "Default == mempty" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soExperimentalFeatures) result `Hspec.shouldBe` Right (UUT._default UUT.experimentalFeaturesOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            -- When
            env = [(UUT._envVar UUT.experimentalFeaturesOption, "inherited_roles,optimize_permission_filters,naming_convention")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soExperimentalFeatures) result
          `Hspec.shouldBe` Right (Set.fromList [Types.EFInheritedRoles, Types.EFOptimizePermissionFilters, Types.EFNamingConventions])

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoExperimentalFeatures = Just (Set.fromList [Types.EFInheritedRoles, Types.EFOptimizePermissionFilters, Types.EFNamingConventions])}
            -- When
            env = [(UUT._envVar UUT.experimentalFeaturesOption, "inherited_roles")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soExperimentalFeatures) result
          `Hspec.shouldBe` Right (Set.fromList [Types.EFInheritedRoles, Types.EFOptimizePermissionFilters, Types.EFNamingConventions])

    Hspec.describe "soEventsFetchBatchSize" $ do
      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.eventsFetchBatchSizeOption, "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soEventsFetchBatchSize) result `Hspec.shouldBe` Right $$(refineTH @NonNegative @Int 200)

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEventsFetchBatchSize = Just $$(refineTH @NonNegative @Int 300)}
            -- When
            env = [(UUT._envVar UUT.eventsFetchBatchSizeOption, "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soEventsFetchBatchSize) result `Hspec.shouldBe` Right $$(refineTH @NonNegative @Int 300)

    Hspec.describe "soGracefulShutdownTimeout" $ do
      Hspec.it "Default == 60" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soGracefulShutdownTimeout) result `Hspec.shouldBe` Right (UUT._default UUT.gracefulShutdownOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            -- When
            env = [(UUT._envVar UUT.gracefulShutdownOption, "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soGracefulShutdownTimeout) result `Hspec.shouldBe` Right $$(refineTH @NonNegative @Seconds 200)

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoGracefulShutdownTimeout = Just $$(refineTH @NonNegative @Seconds 300)}
            -- When
            env = [(UUT._envVar UUT.gracefulShutdownOption, "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soGracefulShutdownTimeout) result `Hspec.shouldBe` Right $$(refineTH @NonNegative @Seconds 300)

    Hspec.describe "soWebSocketConnectionInitTimeout" $ do
      Hspec.it "Default == 3" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soWebSocketConnectionInitTimeout) result `Hspec.shouldBe` Right (UUT._default UUT.webSocketConnectionInitTimeoutOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            env = [(UUT._envVar UUT.webSocketConnectionInitTimeoutOption, "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soWebSocketConnectionInitTimeout) result `Hspec.shouldBe` Right (UUT.WSConnectionInitTimeout $$(refineTH @NonNegative @Seconds 200))

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions =
              emptyServeOptionsRaw
                { UUT.rsoWebSocketConnectionInitTimeout = Just (UUT.WSConnectionInitTimeout $$(refineTH @NonNegative @Seconds 300))
                }
            -- When
            env = [(UUT._envVar UUT.webSocketConnectionInitTimeoutOption, "200")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soWebSocketConnectionInitTimeout) result `Hspec.shouldBe` Right (UUT.WSConnectionInitTimeout $$(refineTH @NonNegative @Seconds 300))

    Hspec.describe "soEnableMetadataQueryLoggingEnv" $ do
      Hspec.it "Default == MetadataQueryLoggingDisabled" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soEnableMetadataQueryLogging) result `Hspec.shouldBe` Right (UUT._default UUT.enableMetadataQueryLoggingOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            -- When
            env = [(UUT._envVar UUT.enableMetadataQueryLoggingOption, "true")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soEnableMetadataQueryLogging) result `Hspec.shouldBe` Right Logging.MetadataQueryLoggingEnabled

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoEnableMetadataQueryLoggingEnv = Logging.MetadataQueryLoggingEnabled}
            -- When
            env = [(UUT._envVar UUT.enableMetadataQueryLoggingOption, "false")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soEnableMetadataQueryLogging) result `Hspec.shouldBe` Right Logging.MetadataQueryLoggingEnabled

    Hspec.describe "soDefaultNamingConvention" $ do
      Hspec.it "Default = HasuraCase" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soDefaultNamingConvention) result `Hspec.shouldBe` Right NamingCase.HasuraCase

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            -- When
            env = [(UUT._envVar UUT.defaultNamingConventionOption, "graphql-default")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soDefaultNamingConvention) result `Hspec.shouldBe` Right NamingCase.GraphqlCase

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoDefaultNamingConvention = Just NamingCase.GraphqlCase}
            -- When
            env = [(UUT._envVar UUT.defaultNamingConventionOption, "hasura-default")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap (UUT.soDefaultNamingConvention) result `Hspec.shouldBe` Right NamingCase.GraphqlCase

    Hspec.describe "soExtensionsSchema" $ do
      Hspec.it "Default == 'public' " $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            -- When
            env = []
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soExtensionsSchema result `Hspec.shouldBe` Right (UUT._default UUT.metadataDBExtensionsSchemaOption)

      Hspec.it "Env > Nothing" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw
            -- When
            -- When
            env = [(UUT._envVar UUT.metadataDBExtensionsSchemaOption, "private")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soExtensionsSchema result `Hspec.shouldBe` Right (MonadTx.ExtensionsSchema "private")

      Hspec.it "Arg > Env" $ do
        let -- Given
            rawServeOptions = emptyServeOptionsRaw {UUT.rsoExtensionsSchema = Just (MonadTx.ExtensionsSchema "other")}
            -- When
            env = [(UUT._envVar UUT.metadataDBExtensionsSchemaOption, "private")]
            -- Then
            result = UUT.runWithEnv env (UUT.mkServeOptions @Hasura rawServeOptions)

        fmap UUT.soExtensionsSchema result `Hspec.shouldBe` Right (MonadTx.ExtensionsSchema "other")
