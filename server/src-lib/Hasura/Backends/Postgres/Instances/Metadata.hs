{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.Backends.Postgres.Instances.Metadata () where


import           Hasura.Prelude

import qualified Data.HashMap.Strict                                    as Map

import           Data.Text.Extended

import qualified Hasura.Backends.Postgres.DDL                           as PG

import           Hasura.Backends.Postgres.SQL.Types                     (QualifiedTable)
import           Hasura.Backends.Postgres.Types.CitusExtraTableMetadata
import           Hasura.Base.Error
import           Hasura.RQL.Types.Backend                               (Backend)
import           Hasura.RQL.Types.Metadata.Backend
import           Hasura.RQL.Types.Relationship
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Backend


--------------------------------------------------------------------------------
-- PostgresMetadata

-- | We differentiate the handling of metadata between Citus and Vanilla
-- Postgres because Citus imposes limitations on the types of joins that it
-- permits, which then limits the types of relations that we can track.
class PostgresMetadata (pgKind :: PostgresKind) where
  -- TODO: find a better name
  validateRel
    :: MonadError QErr m
    => TableCache ('Postgres pgKind)
    -> QualifiedTable
    -> Either (ObjRelDef ('Postgres pgKind)) (ArrRelDef ('Postgres pgKind))
    -> m ()

instance PostgresMetadata 'Vanilla where
  validateRel _ _ _ = pure ()

instance PostgresMetadata 'Citus where
  validateRel
    :: forall m
     . MonadError QErr m
    => TableCache ('Postgres 'Citus)
    -> QualifiedTable
    -> Either (ObjRelDef ('Postgres 'Citus)) (ArrRelDef ('Postgres 'Citus))
    -> m ()
  validateRel tableCache sourceTable relInfo = do
    sourceTableInfo <- lookupTableInfo sourceTable
    case relInfo of
      Left (RelDef _ obj _) ->
        case obj of
          RUFKeyOn (SameTable _)               -> pure ()
          RUFKeyOn (RemoteTable targetTable _) -> checkObjectRelationship sourceTableInfo targetTable
          RUManual RelManualConfig { }         -> pure ()
      Right (RelDef _ obj _) ->
        case obj of
          RUFKeyOn (ArrRelUsingFKeyOn targetTable _col) -> checkArrayRelationship sourceTableInfo targetTable
          RUManual RelManualConfig { }                  -> pure ()
    where
      lookupTableInfo tableName = Map.lookup tableName tableCache
        `onNothing` throw400 NotFound ("no such table " <>> tableName)

      checkObjectRelationship sourceTableInfo targetTable = do
        targetTableInfo <- lookupTableInfo targetTable
        let notSupported = throwNotSupportedError sourceTableInfo targetTableInfo "object"
        case ( _tciExtraTableMetadata $ _tiCoreInfo sourceTableInfo
             , _tciExtraTableMetadata $ _tiCoreInfo targetTableInfo
             ) of
          (Distributed{}, Local)         -> notSupported
          (Distributed{}, Reference)     -> pure ()
          (Distributed{}, Distributed{}) -> pure ()
          (_,             Distributed{}) -> notSupported
          (_,             _)             -> pure ()

      checkArrayRelationship sourceTableInfo targetTable = do
        targetTableInfo <- lookupTableInfo targetTable
        let notSupported = throwNotSupportedError sourceTableInfo targetTableInfo "array"
        case ( _tciExtraTableMetadata $ _tiCoreInfo sourceTableInfo
             , _tciExtraTableMetadata $ _tiCoreInfo targetTableInfo
             ) of
          (Distributed{}, Distributed{}) -> pure ()
          (Distributed{}, _)             -> notSupported
          (_,             Distributed{}) -> notSupported
          (_,             _)             -> pure ()

      showDistributionType :: ExtraTableMetadata -> Text
      showDistributionType = \case
        Local         -> "local"
        Distributed _ -> "distributed"
        Reference     -> "reference"

      throwNotSupportedError :: TableInfo ('Postgres 'Citus) -> TableInfo ('Postgres 'Citus) -> Text -> m ()
      throwNotSupportedError sourceTableInfo targetTableInfo t =
        let tciSrc = _tiCoreInfo sourceTableInfo
            tciTgt = _tiCoreInfo targetTableInfo
        in
        throw400 NotSupported (
          showDistributionType (_tciExtraTableMetadata tciSrc) <>
          " tables (" <> toTxt (_tciName tciSrc) <> ") cannot have an " <> t <> " relationship against a " <>
          showDistributionType (_tciExtraTableMetadata $ _tiCoreInfo targetTableInfo) <>
          " table (" <> toTxt (_tciName tciTgt) <> ")")


----------------------------------------------------------------
-- BackendMetadata instance

instance
  ( Backend ('Postgres pgKind)
  , PostgresMetadata pgKind
  , PG.ToMetadataFetchQuery pgKind
  ) => BackendMetadata ('Postgres pgKind) where
  buildComputedFieldInfo     = PG.buildComputedFieldInfo
  fetchAndValidateEnumValues = PG.fetchAndValidateEnumValues
  resolveSourceConfig        = PG.resolveSourceConfig
  resolveDatabaseMetadata    = PG.resolveDatabaseMetadata
  createTableEventTrigger    = PG.createTableEventTrigger
  buildEventTriggerInfo      = PG.buildEventTriggerInfo @pgKind
  parseBoolExpOperations     = PG.parseBoolExpOperations
  buildFunctionInfo          = PG.buildFunctionInfo
  updateColumnInEventTrigger = PG.updateColumnInEventTrigger
  parseCollectableType       = PG.parseCollectableType
  postDropSourceHook         = PG.postDropSourceHook
  validateRelationship       = validateRel @pgKind
