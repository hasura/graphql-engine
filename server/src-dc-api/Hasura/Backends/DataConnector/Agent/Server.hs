module Hasura.Backends.DataConnector.Agent.Server
  ( dcReferenceServer,
    dcMockableServer,
    withDcServer,
    runDcServer,
  )
where

--------------------------------------------------------------------------------

import Control.Concurrent qualified as C
import Control.Exception (bracket)
import Control.Monad.Except (liftIO)
import Data.OpenApi qualified as OpenApi
import Data.Proxy
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Agent.Data
import Hasura.Backends.DataConnector.Agent.Query
import Hasura.Backends.DataConnector.Agent.Schema
import Network.Wai.Handler.Warp qualified as Warp
import Servant
import Prelude

--------------------------------------------------------------------------------

capabilitiesHandler :: Handler API.CapabilitiesResponse
capabilitiesHandler =
  pure $
    API.CapabilitiesResponse
      { crCapabilities =
          API.Capabilities
            { API.cQueries = Just API.QueryCapabilities {API.qcSupportsPrimaryKeys = True},
              API.cMutations = Nothing,
              API.cSubscriptions = Nothing,
              API.cFiltering = Nothing,
              API.cRelationships = Just API.RelationshipCapabilities {}
            },
        crConfigSchemaResponse =
          API.ConfigSchemaResponse
            { _csrConfigSchema =
                mempty
                  { OpenApi._schemaType = Just OpenApi.OpenApiObject,
                    OpenApi._schemaNullable = Just False
                  },
              _csrOtherSchemas = mempty
            }
      }

dcReferenceServer :: Server API.Api
dcReferenceServer = capabilitiesHandler :<|> schemaHandler :<|> queryHandler staticData

-- TODO(SOLOMON):
dcMockableServer :: Server API.Api
dcMockableServer = undefined

runDcServer :: IO ()
runDcServer =
  let app = serve (Proxy :: Proxy API.Api) dcReferenceServer
   in Warp.run 8100 app

withDcServer :: Server API.QueryApi -> IO () -> IO ()
withDcServer server action =
  let app = serve (Proxy :: Proxy API.QueryApi) server
   in bracket
        (liftIO $ C.forkIO $ Warp.run 8100 app)
        C.killThread
        (const action)
