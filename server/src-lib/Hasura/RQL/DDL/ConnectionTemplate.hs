-- | ConnectionTemplate
--
-- This module defines the needed types/functions for implementing the metadata
-- API `<backend>_test_connection_template`.
module Hasura.RQL.DDL.ConnectionTemplate
  ( runTestConnectionTemplate,
    TestConnectionTemplate (..),
    BackendResolvedConnectionTemplate (..),
    ResolvedConnectionTemplateWrapper (..),
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson qualified as J
import Hasura.Backends.Postgres.Connection.Settings (ConnectionTemplate (..))
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata (MetadataM)
import Hasura.RQL.Types.SchemaCache (CacheRM, askSourceConfig)
import Hasura.SQL.AnyBackend qualified as AB

-- | The input type for the metadata API `<backend>_test_connection_template`
data TestConnectionTemplate b = TestConnectionTemplate
  { _tctSourceName :: SourceName,
    _tctRequestContext :: ConnectionTemplateRequestContext b,
    _tctConnectionTemplate :: (Maybe ConnectionTemplate)
  }

instance (Backend b) => FromJSON (TestConnectionTemplate b) where
  parseJSON v =
    flip (J.withObject "TestConnectionTemplate") v $ \o ->
      TestConnectionTemplate
        <$> o
        J..:? "source_name"
        J..!= defaultSource
        <*> o
        J..: "request_context"
        <*> o
        J..:? "connection_template"

-- | Resolver for the metadata API `<backend>_test_connection_template`
runTestConnectionTemplate ::
  forall b m.
  (MonadError QErr m, CacheRM m, Backend b, MetadataM m) =>
  TestConnectionTemplate b ->
  m EncJSON
runTestConnectionTemplate (TestConnectionTemplate sourceName requestContext connectionTemplateMaybe) = do
  sourceConfig <- askSourceConfig @b sourceName
  liftEither $ resolveConnectionTemplate @b sourceConfig requestContext connectionTemplateMaybe

-- A wrapper around the `ResolvedConnectionTemplate` for adding this to `query-log`
newtype ResolvedConnectionTemplateWrapper b = ResolvedConnectionTemplateWrapper
  { getResolvedConnectionTemplateWrapper :: ResolvedConnectionTemplate b
  }

-- An AnyBackend wrapper around `ResolvedConnectionTemplateWrapper` for logging as query-log
newtype BackendResolvedConnectionTemplate = BackendResolvedConnectionTemplate
  { getBackendResolvedConnectionTemplate :: AB.AnyBackend ResolvedConnectionTemplateWrapper
  }

instance ToJSON BackendResolvedConnectionTemplate where
  toJSON resolvedConnectionTemplate =
    AB.dispatchAnyBackend
      @Backend
      (getBackendResolvedConnectionTemplate resolvedConnectionTemplate)
      $ \(resolvedConnectionTemplate' :: ResolvedConnectionTemplateWrapper b) ->
        J.object
          [ "result" J..= getResolvedConnectionTemplateWrapper resolvedConnectionTemplate'
          ]
