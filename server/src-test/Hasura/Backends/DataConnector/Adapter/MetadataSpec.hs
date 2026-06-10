module Hasura.Backends.DataConnector.Adapter.MetadataSpec (spec) where

import Data.Text qualified as Text
import Hasura.Backends.DataConnector.Adapter.Metadata (getDataConnectorInfo, isConnectionError)
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC
import Hasura.Base.Error (Code (..), QErr (..), err400, err500)
import Hasura.Prelude
import Hasura.RQL.Types.DataConnector (mkDataConnectorName)
import Language.GraphQL.Draft.Syntax qualified as GQL
import Test.Hspec

mkName :: Text -> DC.DataConnectorName
mkName n =
  either error id
    $ maybe (Left ("invalid GQL name: " <> Text.unpack n)) mkDataConnectorName (GQL.mkName n)

spec :: Spec
spec = do
  describe "isConnectionError" $ do
    it "is True for a ConnectionNotEstablished error (agent unreachable)" $
      isConnectionError (err500 ConnectionNotEstablished "boom") `shouldBe` True

    it "is False for other error codes (e.g. a reachable agent returning a bad status)" $ do
      isConnectionError (err500 DataConnectorError "agent returned 500") `shouldBe` False
      isConnectionError (err400 DataConnectorError "bad capabilities payload") `shouldBe` False
      isConnectionError (err400 Unexpected "something else") `shouldBe` False

  describe "getDataConnectorInfo" $ do
    it "throws an actionable error when the connector is missing from the backend info" $ do
      -- This is the production failure mode: a source references a connector
      -- that was dropped from the backend info because the agent was
      -- unreachable during schema introspection.
      let result :: Either QErr DC.DataConnectorInfo
          result = getDataConnectorInfo (mkName "snowflake") mempty
      case result of
        Right _ -> expectationFailure "expected the lookup to fail for a missing connector"
        Left QErr {qeError, qeCode} -> do
          qeCode `shouldBe` DataConnectorError
          -- Backwards-compatible phrasing is retained...
          qeError `shouldSatisfy` ("was not found in the data connector backend info" `Text.isInfixOf`)
          -- ...and the message names the connector and is actionable.
          qeError `shouldSatisfy` ("snowflake" `Text.isInfixOf`)
          qeError `shouldSatisfy` ("reload the metadata" `Text.isInfixOf`)
