-- | This module houses the types that are necessary to even talk about native
-- queries abstract of a concrete implementation.
--
-- The default implementation is given in modules
-- 'Hasura.NativeQuery.Metadata', and 'Hasura.NativeQuery.API', but backends
-- are free to provide their own as needed.
module Hasura.NativeQuery.Types
  ( NativeQueryMetadata (..),
    NativeQueryParseError (..),
    BackendTrackNativeQuery (..),
  )
where

import Autodocodec
import Data.Aeson
import Data.Kind
import Data.Text.Extended (ToTxt)
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.SQL.Backend

type Representable a = (Show a, Eq a, Hashable a, NFData a)

type APIType a = (ToJSON a, FromJSON a)

-- | This type class models the types and functions necessary to talk about
-- Native Queries.
--
-- Uninstantiable defaults are given for types and methods.
class
  ( APIType (NativeQueryName b),
    APIType (TrackNativeQuery b),
    APIType (NativeQueryInfo b),
    Ord (NativeQueryName b),
    HasCodec (NativeQueryInfo b),
    Representable (NativeQueryInfo b),
    Representable (NativeQueryName b),
    ToTxt (NativeQueryName b)
  ) =>
  NativeQueryMetadata (b :: BackendType)
  where
  -- | The type of persisted metadata.
  type NativeQueryInfo b :: Type

  type NativeQueryInfo b = Void

  -- | The types of names of native queries.
  type NativeQueryName b :: Type

  type NativeQueryName b = Void

  -- | The API payload of the 'track_native_query' api endpoint.
  type TrackNativeQuery b :: Type

  type TrackNativeQuery b = Void

  -- | Projection function identifying the name of the source a 'track_native_query' request concerns.
  trackNativeQuerySource :: TrackNativeQuery b -> SourceName
  default trackNativeQuerySource :: (TrackNativeQuery b ~ Void) => TrackNativeQuery b -> SourceName
  trackNativeQuerySource = absurd

  -- | Projection function giving the name of a native query.
  nativeQueryInfoName :: NativeQueryInfo b -> NativeQueryName b
  default nativeQueryInfoName :: (NativeQueryInfo b ~ Void) => NativeQueryInfo b -> NativeQueryName b
  nativeQueryInfoName = absurd

  -- | Projection function, producing a 'NativeQueryInfo b' from a 'TrackNativeQuery b'.
  nativeQueryTrackToInfo :: TrackNativeQuery b -> Either NativeQueryParseError (NativeQueryInfo b)
  default nativeQueryTrackToInfo :: (TrackNativeQuery b ~ Void) => TrackNativeQuery b -> Either NativeQueryParseError (NativeQueryInfo b)
  nativeQueryTrackToInfo = absurd

-- | Our API endpoint solution wraps all request payload types in 'AnyBackend'
-- for its multi-backend support, but type families must be fully applied to
-- all their arguments.
--
-- So in order to be usable as an API request payload data type,
-- 'TrackNativeQuery b' needs to be wrapped in a newtype.
newtype BackendTrackNativeQuery b = BackendTrackNativeQuery {getBackendTrackNativeQuery :: TrackNativeQuery b}

deriving newtype instance NativeQueryMetadata b => FromJSON (BackendTrackNativeQuery b)

-- Things that might go wrong when converting a Native Query metadata request
-- into a valid metadata item (such as failure to interpolate the query)
newtype NativeQueryParseError = NativeQueryParseError Text
