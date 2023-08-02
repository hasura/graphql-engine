module Hasura.PingSources
  ( runPingSources,
  )
where

import Control.Concurrent.Extended qualified as Conc
import Data.Environment qualified as Env
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.Source (SourcePingCache, SourcePingInfo (..))
import Hasura.SQL.AnyBackend qualified as AB

-- | A forever running IO loop that performs regular pings for DBs that need it
-- these are used to send a fingerprint to third parties that wish to attribute
-- users to Hasura
runPingSources ::
  Env.Environment ->
  (String -> IO ()) ->
  IO SourcePingCache ->
  IO a
runPingSources env pingLog fetchPingCacheIO =
  forever $ do
    pingCache <- liftIO fetchPingCacheIO
    for_ pingCache $ \someSourcePingInfo ->
      AB.dispatchAnyBackend @Backend
        someSourcePingInfo
        \(thisSourcePingInfo :: SourcePingInfo b) ->
          runPingSource @b
            env
            pingLog
            (_spiName thisSourcePingInfo)
            (_spiConnection thisSourcePingInfo)

    -- Sleep the thread for a minute
    liftIO $ Conc.sleep $ seconds 60
