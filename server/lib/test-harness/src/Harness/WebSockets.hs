module Harness.WebSockets
  ( responseListener,
    sendMessages,
  )
where

import Control.Lens ((^?))
import Data.Aeson (Value (..), eitherDecode, encode)
import Data.Aeson.Lens (key, _String)
import Data.Has (Has)
import Harness.Logging
import Hasura.Prelude
import Network.WebSockets qualified as WS

-- Wait for a server response for the given identifier that is interesting.
responseListener :: (Has Logger env) => env -> WS.Connection -> (Maybe Text -> Text -> Value -> IO r) -> IO r
responseListener testEnv connection yield = do
  message <- WS.receiveData connection
  decoded <- eitherDecode @Value message `onLeft` fail
  testLogMessage testEnv $ LogHGEWebSocketResponse decoded

  fromMaybe (responseListener testEnv connection yield) do
    type_ <- decoded ^? key "type" . _String
    guard $ type_ `notElem` ["ka", "connection_ack", "complete"]

    let identifier = decoded ^? key "id" . _String
    let payload = fromMaybe Null $ decoded ^? key "payload"
    pure $ yield identifier type_ payload

sendMessages :: (Has Logger env) => env -> WS.Connection -> [Value] -> IO ()
sendMessages testEnv connection messages = do
  for_ messages $ \msg -> testLogMessage testEnv $ LogHGEWebSocketRequest msg
  WS.sendTextDatas connection (encode <$> messages)
