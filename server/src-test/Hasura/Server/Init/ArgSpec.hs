{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Hasura.Server.Init.ArgSpec
  ( spec,
  )
where

--------------------------------------------------------------------------------

import Control.Lens (preview, _Just)
import Data.Aeson qualified as J
import Data.HashSet qualified as Set
import Data.Time (NominalDiffTime)
import Data.URL.Template qualified as Template
import Database.PG.Query qualified as PG
import Hasura.GraphQL.Execute.Subscription.Options qualified as ES
import Hasura.Logging qualified as Logging
import Hasura.Prelude
import Hasura.RQL.Types.Metadata (Metadata, MetadataDefaults (..), overrideMetadataDefaults, _metaBackendConfigs)
import Hasura.RQL.Types.NamingCase qualified as NC
import Hasura.RQL.Types.Roles qualified as Roles
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.SQL.BackendMap qualified as BackendMap
import Hasura.Server.Auth qualified as Auth
import Hasura.Server.Cors qualified as Cors
import Hasura.Server.Init qualified as UUT
import Hasura.Server.Logging qualified as Logging
import Hasura.Server.Types qualified as Types
import Network.WebSockets qualified as WS
import Options.Applicative qualified as Opt
import Refined (NonNegative, Positive, refineTH)
import Test.Hspec qualified as Hspec

{-# ANN module ("HLint: ignore Redundant ==" :: String) #-}

--------------------------------------------------------------------------------

spec :: Hspec.Spec
spec = Hspec.describe "Arg Parsing Tests" $ do
  mainParserSpec
  downgradeParserSpec
  serveParserSpec

--------------------------------------------------------------------------------

mainParserSpec :: Hspec.Spec
mainParserSpec =
  Hspec.describe "Main Command" $ do
    Hspec.it "Accepts '--database-url'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.parseHgeOpts @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--database-url", "postgres://user:password@localhost/hasura", "serve", "--server-port", "420"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _do -> pure ()
        Opt.Failure pf -> Hspec.expectationFailure $ show pf
        Opt.CompletionInvoked _cr -> Hspec.expectationFailure "Completion Invoked"

    Hspec.it "Rejects '--database-url' requires an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.parseHgeOpts @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--database-url", "serve", "--server-port", "420"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "Accepts '--database-url' with a valid Template argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.parseHgeOpts @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--database-url", "https://hasura.io/{{foo}}", "serve", "--server-port", "420"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap (preview (UUT.horDatabaseUrl . UUT.pciDatabaseConn . _Just . UUT._PGConnDatabaseUrl)) result `Hspec.shouldSatisfy` \case
        Opt.Success template -> template == eitherToMaybe (Template.parseTemplate "https://hasura.io/{{foo}}")
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "Accepts PostgressConnDetailsRaw flags" $ do
      let -- Given
          parserInfo = Opt.info (UUT.parseHgeOpts @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput =
            [ "--host",
              "localhost",
              "--port",
              "22",
              "--user",
              "user",
              "--password",
              "pass",
              "--dbname",
              "hasura",
              "serve",
              "--server-port",
              "420"
            ]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap (preview (UUT.horDatabaseUrl . UUT.pciDatabaseConn . _Just . UUT._PGConnDetails)) result `Hspec.shouldSatisfy` \case
        Opt.Success template ->
          template
            == Just
              ( UUT.PostgresConnDetailsRaw
                  { connHost = "localhost",
                    connPort = 22,
                    connUser = "user",
                    connPassword = "pass",
                    connDatabase = "hasura",
                    connOptions = Nothing
                  }
              )
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "Accepts '--metadata-database-url'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.parseHgeOpts @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--metadata-database-url", "postgres://user:password@localhost/hasura", "serve", "--server-port", "420"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT._horMetadataDbUrl result `Hspec.shouldSatisfy` \case
        Opt.Success metadataUrl -> metadataUrl == Just "postgres://user:password@localhost/hasura"
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "Accepts the serve command" $ do
      let -- Given
          parserInfo = Opt.info (UUT.parseHgeOpts @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["serve", "--server-port", "420"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput
      case result of
        Opt.Success _do -> pure ()
        Opt.Failure pf -> Hspec.expectationFailure $ show pf
        Opt.CompletionInvoked _cr -> Hspec.expectationFailure "Completion Invoked"

    Hspec.it "Accepts the export command" $ do
      let -- Given
          parserInfo = Opt.info (UUT.parseHgeOpts @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["export"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput
      case result of
        Opt.Success _do -> pure ()
        Opt.Failure pf -> Hspec.expectationFailure $ show pf
        Opt.CompletionInvoked _cr -> Hspec.expectationFailure "Completion Invoked"

    Hspec.it "Accepts the clean command" $ do
      let -- Given
          parserInfo = Opt.info (UUT.parseHgeOpts @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["clean"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput
      case result of
        Opt.Success _do -> pure ()
        Opt.Failure pf -> Hspec.expectationFailure $ show pf
        Opt.CompletionInvoked _cr -> Hspec.expectationFailure "Completion Invoked"

    Hspec.it "Accepts the downgrade command" $ do
      let -- Given
          parserInfo = Opt.info (UUT.parseHgeOpts @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["downgrade", "--to-v1.0.0-beta.1", "--dryRun"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput
      case result of
        Opt.Success _do -> pure ()
        Opt.Failure pf -> Hspec.expectationFailure $ show pf
        Opt.CompletionInvoked _cr -> Hspec.expectationFailure "Completion Invoked"

    Hspec.it "Accepts the version command" $ do
      let -- Given
          parserInfo = Opt.info (UUT.parseHgeOpts @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["version"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput
      case result of
        Opt.Success _do -> pure ()
        Opt.Failure pf -> Hspec.expectationFailure $ show pf
        Opt.CompletionInvoked _cr -> Hspec.expectationFailure "Completion Invoked"

--------------------------------------------------------------------------------

downgradeParserSpec :: Hspec.Spec
downgradeParserSpec =
  Hspec.describe "Downgrade Command" $ do
    Hspec.it "It accepts '--to-catalog-version'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.downgradeCommandParser Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--to-catalog-version", "v2.8.2"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      result `Hspec.shouldSatisfy` \case
        Opt.Success do' -> do' == UUT.DowngradeOptions "v2.8.2" False
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--to-catalog-version' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.downgradeCommandParser Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--to-catalog-version"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--dry-run' with '--to-catalog-version'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.downgradeCommandParser Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--to-catalog-version", "v2.8.2", "--dryRun"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      result `Hspec.shouldSatisfy` \case
        Opt.Success do' -> do' == UUT.DowngradeOptions "v2.8.2" True
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It accepts '--dry-run' with a downgrade shortcut" $ do
      let -- Given
          parserInfo = Opt.info (UUT.downgradeCommandParser Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--to-v1.0.0-beta.1", "--dryRun"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      result `Hspec.shouldSatisfy` \case
        Opt.Success do' -> do' == UUT.DowngradeOptions "16" True
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--dry-run' does not expect an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.downgradeCommandParser Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--to-catalog-version", "v2.8.2", "--dryRun", "x"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--dry-run' must be run with '--to-catalog-version' or a downgrade shortcut" $ do
      let -- Given
          parserInfo = Opt.info (UUT.downgradeCommandParser Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--dryRun"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts a downgrade shortcut" $ do
      let -- Given
          parserInfo = Opt.info (UUT.downgradeCommandParser Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--to-v1.0.0-beta.1"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      result `Hspec.shouldSatisfy` \case
        Opt.Success do' -> do' == UUT.DowngradeOptions "16" False
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails downgrade shortcuts dont expect arguments" $ do
      let -- Given
          parserInfo = Opt.info (UUT.downgradeCommandParser Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--to-v1.0.0-beta.1", "x"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

--------------------------------------------------------------------------------

serveParserSpec :: Hspec.Spec
serveParserSpec =
  Hspec.describe "Serve Command" $ do
    Hspec.it "It accepts '--server-port'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--server-port", "420"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoPort result `Hspec.shouldSatisfy` \case
        Opt.Success port -> port == Just (UUT.unsafePort 420)
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It accepts '--server-port' 0" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--server-port", "0"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoPort result `Hspec.shouldSatisfy` \case
        Opt.Success port -> port == Just (UUT.unsafePort 0)
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--server-port' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--server-port"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--server-port' expects an integer" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--server-port", "four"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--server-port' fails on a negative integer" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--server-port", "-1"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--server-port' fails on > 65535" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--server-port", "65536"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--server-host'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--server-host", "*"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoHost result `Hspec.shouldSatisfy` \case
        Opt.Success host -> host == Just "*"
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--server-host' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--server-host"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts the flags for a 'ConnParamsRaw'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--stripes", "3", "--connections", "2", "--timeout", "40", "--conn-lifetime", "400", "--use-prepared-statements", "true", "--pool-timeout", "45"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoConnParams result `Hspec.shouldSatisfy` \case
        Opt.Success rawConnParams ->
          rawConnParams
            == UUT.ConnParamsRaw
              { rcpStripes = Just $$(refineTH @NonNegative @Int 3),
                rcpConns = Just $$(refineTH @NonNegative @Int 2),
                rcpIdleTime = Just $$(refineTH @NonNegative @Int 40),
                rcpConnLifetime = Just $$(refineTH @NonNegative @NominalDiffTime 400),
                rcpAllowPrepare = Just True,
                rcpPoolTimeout = Just $$(refineTH @NonNegative @NominalDiffTime 45)
              }
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--stripes' must be a non-negative integer" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--stripes", "-3", "--connections", "2", "--timeout", "40", "--conn-lifetime", "400", "--use-prepared-statements", "true", "--pool-timeout", "45"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--connections' must be a non-negative integer" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--stripes", "3", "--connections", "-2", "--timeout", "40", "--conn-lifetime", "400", "--use-prepared-statements", "true", "--pool-timeout", "45"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--timeout' must be a non-negative integer" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--stripes", "3", "--connections", "2", "--timeout", "-40", "--conn-lifetime", "400", "--use-prepared-statements", "true", "--pool-timeout", "45"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--conn-lifetime' must be a non-negative integer" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--stripes", "3", "--connections", "2", "--timeout", "40", "--conn-lifetime", "-400", "--use-prepared-statements", "true", "--pool-timeout", "45"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--pool-timeout' must be a non-negative integer" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--stripes", "3", "--connections", "2", "--timeout", "40", "--conn-lifetime", "400", "--use-prepared-statements", "true", "--pool-timeout", "-45"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--tx-iso'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--tx-iso", "read-committed"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoTxIso result `Hspec.shouldSatisfy` \case
        Opt.Success txIso -> txIso == Just PG.ReadCommitted
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--tx-iso' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--tx-iso"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--tx-iso' expects valid TX isolation levels" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--tx-iso", "foo"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--admin-secret'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--admin-secret", "A monad is a monoid in the category of endofunctors"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoAdminSecret result `Hspec.shouldSatisfy` \case
        Opt.Success adminSecret -> adminSecret == Just (Auth.hashAdminSecret "A monad is a monoid in the category of endofunctors")
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--admin-secret' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--admin-secret"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--access-key'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--access-key", "A monad is a monoid in the category of endofunctors"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoAdminSecret result `Hspec.shouldSatisfy` \case
        Opt.Success adminSecret -> adminSecret == Just (Auth.hashAdminSecret "A monad is a monoid in the category of endofunctors")
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--access-key' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--access-key"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--admin-secret' and '--access-key' cannot be used in conjunction" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--admin-secret", "A monad is a monoid in the category of endofunctors", "--access-key", "x"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--auth-hook'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--auth-hook", "http://www.auth.com"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap (UUT.ahrUrl . UUT.rsoAuthHook) result `Hspec.shouldSatisfy` \case
        Opt.Success ahUrl -> ahUrl == Just "http://www.auth.com"
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--auth-hook' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--auth-hook"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--auth-hook-mode'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--auth-hook-mode", "POST"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap (UUT.ahrType . UUT.rsoAuthHook) result `Hspec.shouldSatisfy` \case
        Opt.Success ahType -> ahType == Just Auth.AHTPost
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--auth-hook-mode' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--auth-hook-mode"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--auth-hook-mode' only expects GET or POST" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--auth-hook-mode", "PUT"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--jwt-secret'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--jwt-secret", "{ \"jwk_url\": \"https://www.hasura.io\", \"issuer\": \"myapp\" }"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoJwtSecret result `Hspec.shouldSatisfy` \case
        Opt.Success jwtSecret -> jwtSecret == either (const Nothing) Just (readJson "{ \"jwk_url\": \"https://www.hasura.io\", \"issuer\": \"myapp\" }")
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--jwt-secret' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--jwt-secret"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--jwt-secret' expects a JSON serialized 'JWTConfig' object" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--jwt-secret", "{ \"name\": \"Simon\", \"issuer\": \"myapp\" }"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--unauthorized-role'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--unauthorized-role", "guest"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoUnAuthRole result `Hspec.shouldSatisfy` \case
        Opt.Success unAuthRole -> fmap Roles.roleNameToTxt unAuthRole == Just "guest"
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--unauthorized-role' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--unauthorized-role"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--cors-domain'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--cors-domain", "https://*.foo.bar.com:8080, http://*.localhost, http://localhost:3000, http://example.com"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoCorsConfig result `Hspec.shouldSatisfy` \case
        Opt.Success corsConfig ->
          corsConfig == eitherToMaybe (Cors.readCorsDomains "https://*.foo.bar.com:8080, http://*.localhost, http://localhost:3000, http://example.com")
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--cors-domain' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--cors-domain"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--cors-domain' rejects an invalid CORS domains" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--cors-domain", "x"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--disable-cors'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--disable-cors"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoCorsConfig result `Hspec.shouldSatisfy` \case
        Opt.Success corsConfig -> corsConfig == Just (Cors.CCDisabled False)
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It accepts '--disable-cors' supercedes '--cors-domain'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--cors-domain", "https://*.foo.bar.com:8080, http://*.localhost, http://localhost:3000, http://example.com", "--disable-cors"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoCorsConfig result `Hspec.shouldSatisfy` \case
        Opt.Success corsConfig -> corsConfig == Just (Cors.CCDisabled False)
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--disable-cors' does not expect an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--disable-cors", "x"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--enable-console'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--enable-console"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoConsoleStatus result `Hspec.shouldSatisfy` \case
        Opt.Success enableConsole -> enableConsole == UUT.ConsoleEnabled
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--enable-console' does not expect an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--enable-console", "x"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--console-assets-dir'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--console-assets-dir", "/assets"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoConsoleAssetsDir result `Hspec.shouldSatisfy` \case
        Opt.Success consoleAssetsDir -> consoleAssetsDir == Just "/assets"
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--console-assets-dir' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--console-assets-dir"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--console-sentry-dsn'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--console-sentry-dsn", "123123"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoConsoleSentryDsn result `Hspec.shouldSatisfy` \case
        Opt.Success consoleSentryDsn -> consoleSentryDsn == Just "123123"
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--console-sentry-dsn' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--console-sentry-dsn"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--enable-telemetry'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--enable-telemetry", "true"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoEnableTelemetry result `Hspec.shouldSatisfy` \case
        Opt.Success enableTelemetry -> enableTelemetry == Just UUT.TelemetryEnabled
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--enable-telemetry' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--enable-telemetry"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--enable-telemetry' expects a boolean argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--enable-telemetry", "one"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--ws-read-cookie'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--ws-read-cookie"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoWsReadCookie result `Hspec.shouldSatisfy` \case
        Opt.Success wsReadCookie -> wsReadCookie == UUT.WsReadCookieEnabled
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--ws-read-cookie' does not expect an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--ws-read-cookie", "x"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--stringify-numeric-types'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--stringify-numeric-types"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoStringifyNum result `Hspec.shouldSatisfy` \case
        Opt.Success stringifyNum -> stringifyNum == Options.StringifyNumbers
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--stringify-numeric-types' does not expect an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--stringify-numeric-types", "x"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--v1-boolean-null-collapse'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--v1-boolean-null-collapse", "true"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoDangerousBooleanCollapse result `Hspec.shouldSatisfy` \case
        Opt.Success dangerousBooleanCollapse -> dangerousBooleanCollapse == Just Options.DangerouslyCollapseBooleans
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--v1-boolean-null-collapse' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--v1-boolean-null-collapse"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--v1-boolean-null-collapse' expects a boolean argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--v1-boolean-null-collapse", "123"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--enabled-apis'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--enabled-apis", "graphql,pgdump"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoEnabledAPIs result `Hspec.shouldSatisfy` \case
        Opt.Success enabledAPIs -> enabledAPIs == Just (Set.fromList [UUT.GRAPHQL, UUT.PGDUMP])
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--enabled-apis' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--enabled-apis"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--enabled-apis' expects a valid API arguments" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--enabled-apis", "PIZZA"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--live-queries-multiplexed-refetch-interval'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--live-queries-multiplexed-refetch-interval", "54000"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoMxRefetchInt result `Hspec.shouldSatisfy` \case
        Opt.Success mxRefetchInt -> mxRefetchInt == ES.mkRefetchInterval 54
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--live-queries-multiplexed-refetch-interval' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--live-queries-multiplexed-refetch-interval"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--live-queries-multiplexed-refetch-interval' expects an integer argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--live-queries-multiplexed-refetch-interval", "true"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--live-queries-multiplexed-batch-size'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--live-queries-multiplexed-batch-size", "102"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoMxBatchSize result `Hspec.shouldSatisfy` \case
        Opt.Success mxBatchSize -> mxBatchSize == ES.mkBatchSize 102
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--live-queries-multiplexed-batch-size' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--live-queries-multiplexed-batch-size"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--live-queries-multiplexed-batch-size' expects an integer argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--live-queries-multiplexed-batch-size", "false"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--streaming-queries-multiplexed-refetch-interval'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--streaming-queries-multiplexed-refetch-interval", "57000"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoStreamingMxRefetchInt result `Hspec.shouldSatisfy` \case
        Opt.Success streamingMxRefetchInt -> streamingMxRefetchInt == ES.mkRefetchInterval 57
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--streaming-queries-multiplexed-refetch-interval' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--streaming-queries-multiplexed-refetch-interval"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--streaming-queries-multiplexed-refetch-interval' expects an integer argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--streaming-queries-multiplexed-refetch-interval", "foo"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--streaming-queries-multiplexed-batch-size'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--streaming-queries-multiplexed-batch-size", "102"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoStreamingMxBatchSize result `Hspec.shouldSatisfy` \case
        Opt.Success streamingMxBatchSize -> streamingMxBatchSize == ES.mkBatchSize 102
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--streaming-queries-multiplexed-batch-size' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--streaming-queries-multiplexed-batch-size"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--streaming-queries-multiplexed-batch-size' expects an integer argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--streaming-queries-multiplexed-batch-size", "102.5"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--enable-allowlist'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--enable-allowlist"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoEnableAllowList result `Hspec.shouldSatisfy` \case
        Opt.Success enableAllowList -> UUT.isAllowListEnabled enableAllowList
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--enable-allowlist' does not expect an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--enable-allowlist", "x"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--enabled-log-types'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--enabled-log-types", "startup, webhook-log"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoEnabledLogTypes result `Hspec.shouldSatisfy` \case
        Opt.Success enabledLogTypes -> enabledLogTypes == Just (Set.fromList [Logging.ELTStartup, Logging.ELTWebhookLog])
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--enabled-log-types' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--enabled-log-types"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--enabled-log-types' expects a valid log types as arguments" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--enabled-log-types", "not-valid-log-type, startup"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--log-level'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--log-level", "warn"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoLogLevel result `Hspec.shouldSatisfy` \case
        Opt.Success logLevel -> logLevel == Just Logging.LevelWarn
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--log-level' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--log-level"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--log-level' expects a valid log level argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--log-level", "shout"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--query-plan-cache-size'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--query-plan-cache-size", "65535"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        -- NOTE: This is deprecated flag whose value is dropped
        Opt.Success _logLevel -> True
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--query-plan-cache-size' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--query-plan-cache-size"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--query-plan-cache-size' expects an integer argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--query-plan-cache-size", "false"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--dev-mode'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--dev-mode"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoDevMode result `Hspec.shouldSatisfy` \case
        Opt.Success devMode -> UUT.isDevModeEnabled devMode
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--dev-mode' does not expect an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--dev-mode", "x"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--admin-internal-errors'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--admin-internal-errors", "true"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoAdminInternalErrors result `Hspec.shouldSatisfy` \case
        Opt.Success adminInternalErrors -> adminInternalErrors == Just UUT.AdminInternalErrorsEnabled
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--admin-internal-errors' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--admin-internal-errors"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--admin-internal-errors' expects a boolean argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--admin-internal-errors", "five"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--events-http-pool-size'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--events-http-pool-size", "50"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoEventsHttpPoolSize result `Hspec.shouldSatisfy` \case
        Opt.Success eventsHttpPoolSize -> eventsHttpPoolSize == Just $$(refineTH @Positive @Int 50)
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--events-http-pool-size' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--events-http-pool-size"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--events-http-pool-size' expects an integer argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--events-http-pool-size", "10.5"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--events-fetch-interval'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--events-fetch-interval", "634"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoEventsFetchInterval result `Hspec.shouldSatisfy` \case
        Opt.Success eventsFetchInterval -> eventsFetchInterval == Just $$(refineTH @NonNegative @Milliseconds 634)
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--events-fetch-interval' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--events-fetch-interval"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--events-fetch-interval' expects an integer argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--events-fetch-interval", "true"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--events-fetch-interval' expects an non-negative integer" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--events-fetch-interval", "-10"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--async-actions-fetch-interval'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--async-actions-fetch-interval", "123"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoAsyncActionsFetchInterval result `Hspec.shouldSatisfy` \case
        Opt.Success asyncActionsFetchInterval -> asyncActionsFetchInterval == Just (UUT.Interval $$(refineTH 123))
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--async-actions-fetch-interval' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--async-actions-fetch-interval"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--async-actions-fetch-interval' expects an integer argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--async-actions-fetch-interval", "true"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--async-actions-fetch-interval' expects a non-negative integer argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--async-actions-fetch-interval", "-10"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--enable-remote-schema-permissions'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--enable-remote-schema-permissions"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoEnableRemoteSchemaPermissions result `Hspec.shouldSatisfy` \case
        Opt.Success enableRemoteSchemaPermissions -> enableRemoteSchemaPermissions == Options.EnableRemoteSchemaPermissions
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--enable-remote-schema-permissions' does not expect an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--enable-remote-schema-permissions", "x"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--websocket-compression'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--websocket-compression"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoWebSocketCompression result `Hspec.shouldSatisfy` \case
        Opt.Success webSocketCompression -> webSocketCompression == WS.PermessageDeflateCompression WS.defaultPermessageDeflate
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--websocket-compression' does not expect an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--websocket-compression", "x"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--websocket-keepalive'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--websocket-keepalive", "8"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoWebSocketKeepAlive result `Hspec.shouldSatisfy` \case
        Opt.Success webSocketKeepAlive -> webSocketKeepAlive == Just (UUT.KeepAliveDelay $$(refineTH 8))
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--websocket-keepalive' expects a non-negative integer" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--websocket-keepalive", "-10"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--websocket-keepalive' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--websocket-keepalive"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--websocket-keepalive' expects an integer argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--websocket-keepalive", "true"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--infer-function-permissions'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--infer-function-permissions", "false"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoInferFunctionPermissions result `Hspec.shouldSatisfy` \case
        Opt.Success inferFunctionPermissions -> inferFunctionPermissions == Just Options.Don'tInferFunctionPermissions
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--infer-function-permissions' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--infer-function-permissions"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--infer-function-permissions' expects a boolean argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--infer-function-permissions", "five"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--enable-maintenance-mode'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--enable-maintenance-mode"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoEnableMaintenanceMode result `Hspec.shouldSatisfy` \case
        Opt.Success enableMaintenanceMode -> enableMaintenanceMode == Types.MaintenanceModeEnabled ()
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--enable-maintenance-mode' does not expect an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--enable-maintenance-mode", "x"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--schema-sync-poll-interval'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--schema-sync-poll-interval", "5432"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoSchemaPollInterval result `Hspec.shouldSatisfy` \case
        Opt.Success schemaPollInterval -> schemaPollInterval == Just (UUT.Interval $$(refineTH 5432))
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--schema-sync-poll-interval' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--schema-sync-poll-interval"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--schema-sync-poll-interval' expects an integer argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--schema-sync-poll-interval", "true"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--schema-sync-poll-interval' expects a non-negative integer argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--schema-sync-poll-interval", "-10"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--experimental-features'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--experimental-features", "inherited_roles,optimize_permission_filters"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoExperimentalFeatures result `Hspec.shouldSatisfy` \case
        Opt.Success schemaPollInterval -> schemaPollInterval == Just (Set.fromList [Types.EFInheritedRoles, Types.EFOptimizePermissionFilters])
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--experimental-features' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--experimental-features"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--experimental-features' expects a valid experimental feature options in the argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--experimental-features", "inherited_roles,pretend_feature"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--events-fetch-batch-size'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--events-fetch-batch-size", "40"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoEventsFetchBatchSize result `Hspec.shouldSatisfy` \case
        Opt.Success eventsFetchBatchSize -> eventsFetchBatchSize == Just $$(refineTH @NonNegative @Int 40)
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--events-fetch-batch-size' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--events-fetch-batch-size"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--events-fetch-batch-size' expects a non-negative integer argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--events-fetch-batch-size", "-40"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--graceful-shutdown-timeout'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--graceful-shutdown-timeout", "52"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoGracefulShutdownTimeout result `Hspec.shouldSatisfy` \case
        Opt.Success gracefulShutdownTimeout -> gracefulShutdownTimeout == Just $$(refineTH @NonNegative @Seconds 52)
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--graceful-shutdown-timeout' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--graceful-shutdown-timeout"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--graceful-shutdown-timeout' expects an integer argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--graceful-shutdown-timeout", "true"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--graceful-shutdown-timeout' expects a non-negative integer argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--graceful-shutdown-timeout", "-10"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--websocket-connection-init-timeout'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--websocket-connection-init-timeout", "34"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoWebSocketConnectionInitTimeout result `Hspec.shouldSatisfy` \case
        Opt.Success webSocketConnectionInitTimeout -> webSocketConnectionInitTimeout == Just (UUT.WSConnectionInitTimeout $$(refineTH 34))
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--websocket-connection-init-timeout' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--websocket-connection-init-timeout"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--websocket-connection-init-timeout' expects an integer argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--websocket-connection-init-timeout", "true"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--websocket-connection-init-timeout' expects a non-negative integer argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--websocket-connection-init-timeout", "-10"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--enable-metadata-query-logging'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--enable-metadata-query-logging"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoEnableMetadataQueryLoggingEnv result `Hspec.shouldSatisfy` \case
        Opt.Success enableMetadataQueryLogging -> enableMetadataQueryLogging == Logging.MetadataQueryLoggingEnabled
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--enable-metadata-query-logging' does not expect an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--enable-metadata-query-logging", "x"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--default-naming-convention'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--default-naming-convention", "graphql-default"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoDefaultNamingConvention result `Hspec.shouldSatisfy` \case
        Opt.Success enableMetadataQueryLogging -> enableMetadataQueryLogging == Just NC.GraphqlCase
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It fails '--default-naming-convention' expects an argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--default-naming-convention"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It fails '--default-naming-convention' expects a valid naming convention argument" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--default-naming-convention", "mysterious-default"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      case result of
        Opt.Success _result -> Hspec.expectationFailure "Should not parse successfully"
        Opt.Failure _pf -> pure ()
        Opt.CompletionInvoked cr -> Hspec.expectationFailure $ show cr

    Hspec.it "It accepts '--metadata-defaults'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput =
            [ "--metadata-defaults",
              "{\"backend_configs\": {\"dataconnector\": {\"sqlite\": {\"uri\": \"http://localhost:8100\"}}}}"
            ]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoMetadataDefaults result `Hspec.shouldSatisfy` \case
        Opt.Success (Just (MetadataDefaults md)) -> not (null (BackendMap.elems (_metaBackendConfigs md)))
        Opt.Success Nothing -> False
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It prefers explicit metadata over '--metadata-defaults'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput =
            [ "--metadata-defaults",
              "{\"backend_configs\": {\"dataconnector\": {\"sqlite\": {\"uri\": \"http://x:80\"}, \"mongo\": {\"uri\": \"http://x:81\"}}}}"
            ]
          mdInput =
            "{\"version\": 3, \"sources\": [], \"backend_configs\": {\"dataconnector\": {\"sqlite\": {\"uri\": \"http://x:82\"}, \"db2\": {\"uri\": \"http://x:83\"}}}}"
          mdExpected =
            "{\"version\": 3, \"sources\": [], \"backend_configs\": {\"dataconnector\": { \"mongo\": {\"uri\": \"http://x:81\"}, \"sqlite\": {\"uri\": \"http://x:82\"}, \"db2\": {\"uri\": \"http://x:83\"}}}}"

          -- Then
          argResult = Opt.execParserPure Opt.defaultPrefs parserInfo argInput
          mdResult = J.eitherDecode @Metadata mdInput
          mdExpectedResult = J.eitherDecode @Metadata mdExpected

      fmap UUT.rsoMetadataDefaults argResult `Hspec.shouldSatisfy` \case
        Opt.Success Nothing -> False
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False
        Opt.Success (Just (MetadataDefaults mdd)) ->
          case (mdResult, mdExpectedResult) of
            (Right md, Right mde) ->
              let o = overrideMetadataDefaults md (MetadataDefaults mdd)
               in o == mde
            _ -> False

    Hspec.it "It accepts '--enable-apollo-federation'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--enable-apollo-federation"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoApolloFederationStatus result `Hspec.shouldSatisfy` \case
        Opt.Success enableApolloFederation -> enableApolloFederation == (Just Types.ApolloFederationEnabled)
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False

    Hspec.it "It accepts '--disable-close-websockets-on-metadata-change'" $ do
      let -- Given
          parserInfo = Opt.info (UUT.serveCommandParser @Logging.Hasura Opt.<**> Opt.helper) Opt.fullDesc
          -- When
          argInput = ["--disable-close-websockets-on-metadata-change"]
          -- Then
          result = Opt.execParserPure Opt.defaultPrefs parserInfo argInput

      fmap UUT.rsoCloseWebsocketsOnMetadataChangeStatus result `Hspec.shouldSatisfy` \case
        Opt.Success disableCloseWebsocketsOnMetadataChange -> disableCloseWebsocketsOnMetadataChange == (Just Types.CWMCDisabled)
        Opt.Failure _pf -> False
        Opt.CompletionInvoked _cr -> False
