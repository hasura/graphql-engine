{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}

-- | This module provides an API for suggesting relationships so that
--   the console (or client) does not need to construct and submit relationship queries itself.
--
--   This suggests reciprocal object relationships A -> object -> B -> object -> A if there is a unique
--   constraint on the column(s) in A mapping A->B, and if not then a reciprocal array relationship
--   A -> object -> B -> array -> A is suggested.
--
--   All JSON fields to the main exported function `runSuggestRels` are optional and behave as follows:
--
--   * _srsSource: The source to suggest relationships for - Defaults to `defaultSource`
--   * _srsTables: The tables to suggest relationships between - Defaults to all tables
--   * _srsOmitTracked: Only suggest untracked relationships - Defaults to False
--
--   Autodocodec Codecs instances are implemented for these datatypes.
module Hasura.RQL.DDL.Relationship.Suggest
  ( SuggestRels,
    runSuggestRels,
  )
where

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.Lens (preview)
import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.NonEmpty qualified as MapNE
import Data.HashSet qualified as H
import Data.OpenApi (ToSchema (..))
import Data.Tuple (swap)
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Relationships.Local (RelInfo (riMapping, riTarget), RelMapping (..), RelTarget (..))
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.Table.Cache (ForeignKey, UniqueConstraint, _cName, _fkColumnMapping, _fkConstraint, _fkForeignTable, _ucColumns)
import Witch qualified

-- | Datatype used by Metadata API to represent Request for Suggested Relationships
data SuggestRels b = SuggestRels
  { _srsSource :: SourceName,
    _srsTables :: Maybe [TableName b],
    _srsOmitTracked :: Bool
  }
  deriving (Generic)
  deriving (J.FromJSON, J.ToJSON, ToSchema) via Autodocodec (SuggestRels b)

instance (Backend b) => HasCodec (SuggestRels b) where
  codec =
    object
      "SuggestRels"
      ( SuggestRels
          <$> optionalFieldWithOmittedDefault "source" defaultSource "The source to suggest relationships for - Defaults to 'default'."
          .= _srsSource
            <*> optionalFieldOrNull "tables" "The list of tables to suggest relationships for - Defaults to all tracked tables."
          .= _srsTables
            <*> optionalFieldWithOmittedDefault "omit_tracked" False "Determines if currently tracked relationships should be ommited from suggestions - Defaults to false."
          .= _srsOmitTracked
      )
      <??> ["API call to request suggestions for relationships"]

newtype SuggestedRelationships b = Relationships
  { sRelationships :: [Relationship b]
  }
  deriving (Generic)
  deriving (J.FromJSON, J.ToJSON, ToSchema) via Autodocodec (SuggestedRelationships b)

instance (Backend b) => HasCodec (SuggestedRelationships b) where
  codec =
    object
      "SuggestedRelationships"
      ( Relationships
          <$> requiredField' "relationships"
          .= sRelationships
      )

data Relationship b = Relationship
  { rType :: RelType,
    rFrom :: Mapping b,
    rTo :: Mapping b
  }
  deriving (Generic)
  deriving (J.FromJSON, J.ToJSON, ToSchema) via Autodocodec (Relationship b)

instance (Backend b) => HasCodec (Relationship b) where
  codec =
    object
      "Relationship"
      ( Relationship
          <$> requiredField' "type"
          .= rType
            <*> requiredField' "from"
          .= rFrom
            <*> requiredField' "to"
          .= rTo
      )

data Mapping b = Mapping
  { mTable :: TableName b,
    mColumns :: [ColumnPath b],
    mConstraintName :: Maybe J.Value
  }
  deriving (Generic)
  deriving (J.FromJSON, J.ToJSON, ToSchema) via Autodocodec (Mapping b)

instance (Backend b) => HasCodec (Mapping b) where
  codec =
    object
      "Mapping"
      ( Mapping
          <$> requiredField' "table"
          .= mTable
            <*> requiredField' "columns"
          .= mColumns
            <*> optionalFieldOrNull' "constraint_name"
          .= mConstraintName
      )

--  | Most of the heavy lifting for this module occurs in this function.
--    Suggests reciprocal relationships for foreign keys.
--    Incorporates logic to omit previously-tracked relationships
--    and only considers required tables.
suggestRelsFK ::
  forall b.
  (Backend b) =>
  -- | Omits currently tracked relationships from recommendations if True.
  Bool ->
  HashMap (TableName b) (TableCoreInfo b) ->
  TableName b ->
  HashSet (UniqueConstraint b) ->
  H.HashSet (TableName b, HashMap (ColumnPath b) (ColumnPath b)) ->
  (TableName b -> Bool) ->
  ForeignKey b ->
  [Relationship b]
suggestRelsFK omitTracked tables name uniqueConstraints tracked predicate foreignKey
  | not (predicate name || predicate relatedTableName) = [] -- Neither table appears in tables list
  | isNothing relatedTable = [] -- There is no information for the related table
  | omitTracked = catMaybes [discard toTracked toRelationship, discard fromTracked fromRelationship] -- Discard tracked relationships if that's requested
  | otherwise = [toRelationship, fromRelationship] -- Otherwise, return the reciprocal relationships
  where
    toTracked = H.member (relatedTableName, columnRelationships) tracked
    fromTracked = H.member (name, invert columnRelationships) trackedBack
    toRelationship, fromRelationship :: Relationship b
    toRelationship =
      Relationship
        { rType = ObjRel,
          rFrom = Mapping {mTable = name, mColumns = localColumns, mConstraintName = Just constraintName},
          rTo = Mapping {mTable = relatedTableName, mColumns = relatedColumns, mConstraintName = Nothing}
        }
    fromRelationship =
      Relationship
        { rType = if H.fromList localColumns `H.member` uniqueConstraintColumns then ObjRel else ArrRel,
          rTo = Mapping {mTable = name, mColumns = localColumns, mConstraintName = Just constraintName},
          rFrom = Mapping {mTable = relatedTableName, mColumns = relatedColumns, mConstraintName = Nothing}
        }
    columnRelationships = MapNE.toHashMap $ _fkColumnMapping foreignKey
    localColumns = HashMap.keys columnRelationships
    relatedColumns = HashMap.elems columnRelationships
    uniqueConstraintColumns = H.map (H.map Witch.from . _ucColumns) uniqueConstraints
    relatedTableName = _fkForeignTable foreignKey
    relatedTable = HashMap.lookup relatedTableName tables
    constraintName = J.toJSON (_cName (_fkConstraint foreignKey))
    discard b x = bool Nothing (Just x) (not b)
    invert = HashMap.fromList . map swap . HashMap.toList
    trackedBack =
      H.fromList
        $ mapMaybe (relationships (getRelationshipsInputs @b))
        $ maybe [] (HashMap.elems . _tciFieldInfoMap) relatedTable

-- we're only interested in suggesting table-based relationships for now
getRelationshipsInputs ::
  RelInfo b ->
  Maybe (TableName b, HashMap (ColumnPath b) (ColumnPath b))
getRelationshipsInputs ri =
  case riTarget ri of
    RelTargetTable tn -> Just (tn, unRelMapping $ riMapping ri)
    _ -> Nothing

suggestRelsTable ::
  forall b.
  (Backend b) =>
  Bool ->
  HashMap (TableName b) (TableCoreInfo b) ->
  (TableName b -> Bool) ->
  (TableName b, TableCoreInfo b) ->
  [Relationship b]
suggestRelsTable omitTracked tables predicate (name, table) =
  suggestRelsFK omitTracked tables name constraints tracked predicate =<< toList foreignKeys
  where
    foreignKeys = _tciForeignKeys table
    constraints = _tciUniqueConstraints table
    tracked =
      H.fromList
        $ mapMaybe (relationships (getRelationshipsInputs @b))
        $ HashMap.elems
        $ _tciFieldInfoMap table

relationships :: (RelInfo b1 -> Maybe b2) -> FieldInfo b1 -> Maybe b2
relationships f = (=<<) f . preview _FIRelationship

-- NOTE: This could be grouped by table instead of a list, console stakeholders are happy with this being a list.
suggestRelsResponse ::
  forall b.
  (Backend b) =>
  Bool ->
  HashMap (TableName b) (TableCoreInfo b) ->
  (TableName b -> Bool) ->
  SuggestedRelationships b
suggestRelsResponse omitTracked tables predicate =
  Relationships
    $ suggestRelsTable omitTracked tables predicate
    =<< HashMap.toList tables

tablePredicate :: (Hashable a) => Maybe [a] -> a -> Bool
tablePredicate Nothing _ = True
tablePredicate (Just ns) n = n `H.member` hash
  where
    hash = H.fromList ns

-- | The method invoked when dispatching on metadata calls in POST /v1/metadata
runSuggestRels ::
  forall b m.
  (MonadError QErr m, CacheRWM m, BackendMetadata b) =>
  SuggestRels b ->
  m EncJSON
runSuggestRels (SuggestRels source tablesM omitExistingB) = do
  tableCacheM <- fmap (fmap (_tiCoreInfo)) <$> askTableCache @b source
  case tableCacheM of
    Nothing -> throw500 "Couldn't find any schema source information"
    Just tableCache -> pure $ encJFromJValue $ suggestRelsResponse @b omitExistingB tableCache (tablePredicate tablesM)
