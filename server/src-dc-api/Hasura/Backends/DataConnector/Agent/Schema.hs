module Hasura.Backends.DataConnector.Agent.Schema
  ( schemaHandler,
  )
where

--------------------------------------------------------------------------------

import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Agent.Data
import Servant.Server
import Prelude

--------------------------------------------------------------------------------

schemaHandler :: API.Config -> Handler API.SchemaResponse
schemaHandler _config = pure schema
