{-# LANGUAGE DeriveAnyClass #-}

module Hasura.RQL.DDL.SchemaRegistry
  ( SchemaProjectId (..),
    IsMetadataInconsistent (..),
    SchemaSDL (..),
    SchemaHash (..),
    ProjectGQLSchemaInformation (..),
    SchemaRegistryConfig (..),
    SchemaRegistryDetails (..),
    GQLSchemaInformation (..),
    SchemaRegistryContext (..),
    SchemaRegistryControlRole (..),
    SchemaRegistryMap,
    SchemaRegistryDetailsList,
    SchemaRegistryAction,
    SchemaRegistryConfigRaw (..),
    calculateSchemaSDLHash,
    selectNowQuery,
  )
where

import Control.Concurrent.STM qualified as STM
import Data.Aeson qualified as J
import Data.Text qualified as T
import Data.Time (UTCTime)
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Execute.Types qualified as SQLTypes
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Metadata (Metadata)
import Hasura.RQL.Types.Metadata.Object (InconsistentMetadata)
import Hasura.RQL.Types.Roles
import Hasura.RQL.Types.SchemaCache (MetadataResourceVersion)
import Hasura.Server.Utils

{-
  SchemaRegistry

  About:
    The schema registry is a feature that is currently meant for
    our users of Hasura Cloud. This feature will help users track
    the evolution of their GraphQL schema across all Hasura roles.
    Additional functionality around tagging schemas and viewing
    the diff of the GraphQL schema across different versions of the
    schema.

    All of the types and other functions that are required for the
    functionality provided by the Hasura server pertaining to this
    feature are present in this file.

  Intended Current Functionality:
    1) Every time the `buildGQLContext` function is run to rebuild
       the GraphQL schema(s), the changes are captured and sent to
       the Hasura PRO server via a TQueue

    2) On the Hasura PRO server, a thread is always maintained to
       read from the aforementioned TQueue for new changes.

    3) The changes that are captured are then processed (more notes
       on this present on `HasuraPro.App` module) and a request with
       all relevant data is sent towards the schema_registry service.

  NOTE: The timestamp recorded as soon as the schema is rebuilt is
        obtained from the Metadata DB. This is done in order to
        make sure that we have only one source for the time to avoid
        clock-skew.

-}

newtype SchemaProjectId = SchemaProjectId {_spiProjectId :: T.Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

newtype IsMetadataInconsistent = IsMetadataInconsistent {_isMdInconsistent :: Bool}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

newtype SchemaSDL = SchemaSDL {_sdl :: T.Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

newtype SchemaHash = SchemaHash {_schemaHash :: T.Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

type SchemaRegistryMap = HashMap RoleName GQLSchemaInformation

type SchemaRegistryAction = Maybe (MetadataResourceVersion -> [InconsistentMetadata] -> Metadata -> IO ())

data GQLSchemaInformation = GQLSchemaInformation
  { _gsiSchemaSDL :: SchemaSDL,
    _gsiSchemaHash :: SchemaHash
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (J.FromJSON)

instance J.ToJSON GQLSchemaInformation where
  toJSON (GQLSchemaInformation schemaSdl schemaHash) =
    J.object
      $ [ "schema_sdl" J..= (_sdl schemaSdl),
          "schema_hash" J..= (_schemaHash schemaHash)
        ]

data ProjectGQLSchemaInformation = ProjectGQLSchemaInformation
  { _pgsiSchemaRegistryMap :: SchemaRegistryMap,
    _pgsiIsMetadataInconsistent :: IsMetadataInconsistent,
    _pgsiAdminSchemaHash :: SchemaHash,
    _pgsiMetadataResourceVersion :: MetadataResourceVersion,
    _pgsiChangeRecordedAt :: UTCTime,
    _pgsiMetadata :: Metadata
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON)

data SchemaRegistryConfig = SchemaRegistryConfig
  { _srcSchemaRegistryWebhook :: T.Text,
    _srcSchemaRegistryAccessKey :: Maybe T.Text
  }
  deriving stock (Eq, Show)

data SchemaRegistryConfigRaw = SchemaRegistryConfigRaw
  { _srcrSchemaRegistryWebhook :: Maybe T.Text,
    _srcrSchemaRegistryAccessKey :: Maybe T.Text
  }
  deriving stock (Eq, Show)

data SchemaRegistryDetails = SchemaRegistryDetails
  { _srdlSchemaRole :: RoleName,
    _srdlSchemaInfo :: GQLSchemaInformation
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (J.FromJSON)

instance J.ToJSON SchemaRegistryDetails where
  toJSON (SchemaRegistryDetails schemaRole schemaInfo) =
    J.object
      [ "hasura_schema_role" J..= schemaRole,
        "schema_info" J..= schemaInfo
      ]

type SchemaRegistryDetailsList = [SchemaRegistryDetails]

-- | Context required to upate schema registry everytime the schema is updated
data SchemaRegistryContext = SchemaRegistryContext
  { _srpaSchemaRegistryTQueueRef :: STM.TQueue ProjectGQLSchemaInformation,
    _srpaMetadataDbPoolRef :: PG.PGPool
  }

newtype SchemaRegistryControlRole = SchemaRegistryControlRole {unSchemaRegistryControlRole :: T.Text}
  deriving stock (Eq, Show)

selectNowQuery :: PG.TxE QErr UTCTime
selectNowQuery =
  runIdentity
    . PG.getRow
    <$> PG.withQE SQLTypes.defaultTxErrorHandler (PG.fromText "SELECT now();") () False

calculateSchemaSDLHash :: T.Text -> RoleName -> SchemaHash
calculateSchemaSDLHash sdl role = SchemaHash $ bsToTxt hash
  where
    hash =
      cryptoHash
        $ J.object
          [ "schema_sdl" J..= sdl,
            "role" J..= roleNameToTxt role
          ]
