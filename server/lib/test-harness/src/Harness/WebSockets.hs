module Harness.WebSockets
  ( responseListener,
  )
where

import Control.Lens ((^?))
import Data.Aeson (Value, eitherDecode)
import Data.Aeson.Lens (key, _String)
import Hasura.Prelude
import Network.WebSockets qualified as WS

-- Wait for a server response for the given identifier that is interesting.
responseListener :: WS.Connection -> (Text -> Text -> Value -> IO r) -> IO r
responseListener connection yield = do
  message <- WS.receiveData connection
  decoded <- eitherDecode @Value message `onLeft` fail

  fromMaybe (responseListener connection yield) do
    identifier <- decoded ^? key "id" . _String

    type_ <- decoded ^? key "type" . _String
    guard $ type_ `notElem` ["ka", "connection_ack", "complete"]

    payload <- decoded ^? key "payload"
    pure $ yield identifier type_ payload
