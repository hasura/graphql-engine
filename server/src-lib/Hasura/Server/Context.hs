module Hasura.Server.Context
  (HttpResponse(..))
  where

import           Hasura.Prelude

import qualified Network.HTTP.Types as HTTP

data HttpResponse a
  = HttpResponse
  { _hrBody    :: !a
  , _hrHeaders :: !HTTP.ResponseHeaders
  } deriving (Functor, Foldable, Traversable)
