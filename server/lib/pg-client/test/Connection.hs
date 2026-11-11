{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Connection (specConnection) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Database.PG.Query.Connection
import Database.PostgreSQL.LibPQ qualified as PQ
import System.Environment qualified as Env
import Test.Hspec
import Prelude

specConnection :: Spec
specConnection =
  describe "PostgreSQL connection initialization" do
    it "sets UTF8 encoding and warning messages by default" $
      withSkipConnectionInitialization Nothing $
        withInitializedConnection latin1NoticeConnectionString $ \conn -> do
          PQ.parameterStatus conn "client_encoding" `shouldReturn` Just "UTF8"
          queryClientMinMessages conn `shouldReturn` "warning"

    it "preserves UTF8 startup parameters when initialization is skipped" $
      withSkipConnectionInitialization (Just "true") $
        withInitializedConnection utf8NoticeConnectionString $ \conn -> do
          PQ.parameterStatus conn "client_encoding" `shouldReturn` Just "UTF8"
          queryClientMinMessages conn `shouldReturn` "notice"

    it "rejects non-UTF8 startup parameters when initialization is skipped" $
      withSkipConnectionInitialization (Just "true") do
        connectionString <- latin1ConnectionString
        initPQConn (connectionInfo connectionString) mempty
          `shouldThrow` (== expectedEncodingError)

    mapM_
      ( \flagValue ->
          it ("retains initialization for flag value " <> show flagValue) $
            withSkipConnectionInitialization (Just flagValue) $
              withInitializedConnection latin1NoticeConnectionString $ \conn -> do
                PQ.parameterStatus conn "client_encoding" `shouldReturn` Just "UTF8"
                queryClientMinMessages conn `shouldReturn` "warning"
      )
      ["false", "TRUE", "invalid"]

withInitializedConnection :: IO ByteString -> (PQ.Connection -> IO a) -> IO a
withInitializedConnection connectionString =
  bracket
    (connectionString >>= \value -> initPQConn (connectionInfo value) mempty)
    PQ.finish

queryClientMinMessages :: PQ.Connection -> IO ByteString
queryClientMinMessages conn = do
  maybeResult <- PQ.exec conn "SHOW client_min_messages"
  case maybeResult of
    Nothing -> expectationFailure "SHOW client_min_messages returned no result" >> pure ""
    Just result -> do
      PQ.resultStatus result `shouldReturn` PQ.TuplesOk
      maybeValue <- PQ.getvalue result 0 0
      case maybeValue of
        Nothing -> expectationFailure "SHOW client_min_messages returned no value" >> pure ""
        Just value -> pure value

connectionInfo :: ByteString -> ConnInfo
connectionInfo connectionString =
  ConnInfo
    { ciRetries = 0,
      ciDetails = CDDatabaseURI connectionString
    }

utf8NoticeConnectionString :: IO ByteString
utf8NoticeConnectionString = connectionStringWithOptions "-c client_encoding=UTF8 -c client_min_messages=NOTICE"

latin1ConnectionString :: IO ByteString
latin1ConnectionString = connectionStringWithOptions "-c client_encoding=LATIN1"

latin1NoticeConnectionString :: IO ByteString
latin1NoticeConnectionString = connectionStringWithOptions "-c client_encoding=LATIN1 -c client_min_messages=NOTICE"

connectionStringWithOptions :: ByteString -> IO ByteString
connectionStringWithOptions options = do
  databaseUrl <- BS.pack <$> Env.getEnv "DATABASE_URL"
  let separator = if BS.elem '?' databaseUrl then "&" else "?"
  pure $ databaseUrl <> separator <> "options=" <> percentEncode options

percentEncode :: ByteString -> ByteString
percentEncode = BS.concatMap encodeByte
  where
    encodeByte ' ' = "%20"
    encodeByte '=' = "%3D"
    encodeByte byte = BS.singleton byte

withSkipConnectionInitialization :: Maybe String -> IO a -> IO a
withSkipConnectionInitialization value action =
  bracket
    (Env.lookupEnv variableName <* setValue value)
    setValue
    (const action)
  where
    variableName = "HASURA_GRAPHQL_SKIP_PG_CONNECTION_INIT"
    setValue = maybe (Env.unsetEnv variableName) (Env.setEnv variableName)

expectedEncodingError :: PGConnErr
expectedEncodingError =
  PGConnErr
    "HASURA_GRAPHQL_SKIP_PG_CONNECTION_INIT=true requires client_encoding=UTF8 in the PostgreSQL connection startup parameters"
