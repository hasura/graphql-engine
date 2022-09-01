module Hasura.Backends.DataConnector.API.V0.Explain
  ( ExplainResponse (..),
  )
where

import Autodocodec
import Autodocodec.OpenAPI ()
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import Prelude

data ExplainResponse = ExplainResponse
  { _erLines :: [Text],
    _erQuery :: Text
  }
  deriving stock (Eq, Ord, Show)
  deriving (ToJSON, FromJSON, ToSchema) via Autodocodec ExplainResponse

instance HasCodec ExplainResponse where
  codec =
    named "ExplainResponse" . object "ExplainResponse" $
      ExplainResponse
        <$> requiredField "lines" "Lines of the formatted explain plan response" .= _erLines
        <*> requiredField "query" "The generated query - i.e. SQL for a relational DB" .= _erQuery
