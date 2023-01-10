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
import Data.Aeson (FromJSON (), ToJSON ())
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.NonEmpty qualified as MapNE
import Data.HashSet qualified as H
import Data.OpenApi (ToSchema (..))
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Relationships.Local (RelInfo (riMapping, riRTable))
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.Table (ForeignKey, UniqueConstraint, _fkColumnMapping, _fkForeignTable, _ucColumns)

-- | Datatype used by Metadata API to represent Request for Suggested Relationships
data SuggestRels b = SuggestRels
  { _srsSource :: SourceName,
    _srsTables :: Maybe [TableName b],
    _srsOmitTracked :: Bool
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec (SuggestRels b)

instance Backend b => HasCodec (SuggestRels b) where
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
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec (SuggestedRelationships b)

instance Backend b => HasCodec (SuggestedRelationships b) where
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
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec (Relationship b)

instance Backend b => HasCodec (Relationship b) where
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
    mColumns :: [Column b]
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec (Mapping b)

instance Backend b => HasCodec (Mapping b) where
  codec =
    object
      "Mapping"
      ( Mapping
          <$> requiredField' "table"
            .= mTable
          <*> requiredField' "columns"
            .= mColumns
      )

--  | Most of the heavy lifting for this module occurs in this function.
--    Suggests reciprocal relationships for foreign keys.
--    Incorporates logic to omit previously-tracked relationships
--    and only considers required tables.
suggestRelsFK ::
  forall b.
  Backend b =>
  -- | Omits currently tracked relationships from recommendations if True.
  Bool ->
  HashMap (TableName b) (TableCoreInfo b) ->
  TableName b ->
  HashSet (UniqueConstraint b) ->
  H.HashSet (TableName b, HashMap (Column b) (Column b)) ->
  ForeignKey b ->
  [Relationship b]
suggestRelsFK omitTracked tables name uniqueConstraints tracked foreignKey =
  case (omitTracked, H.member (relatedTable, columnRelationships) tracked, Map.lookup relatedTable tables) of
    (True, True, _) -> []
    (_, _, Nothing) -> []
    (_, _, Just _) ->
      [ Relationship
          { rType = ObjRel,
            rFrom = Mapping {mTable = name, mColumns = localColumns},
            rTo = Mapping {mTable = relatedTable, mColumns = relatedColumns}
          },
        Relationship
          { rType = if H.fromList localColumns `H.member` uniqueConstraintColumns then ObjRel else ArrRel,
            rTo = Mapping {mTable = name, mColumns = localColumns},
            rFrom = Mapping {mTable = relatedTable, mColumns = relatedColumns}
          }
      ]
  where
    columnRelationships = MapNE.toHashMap (_fkColumnMapping foreignKey)
    localColumns = Map.keys columnRelationships
    relatedColumns = Map.elems columnRelationships
    uniqueConstraintColumns = H.map _ucColumns uniqueConstraints
    relatedTable = _fkForeignTable foreignKey

suggestRelsTable ::
  forall b.
  Backend b =>
  Bool ->
  HashMap (TableName b) (TableCoreInfo b) ->
  (TableName b, TableCoreInfo b) ->
  [Relationship b]
suggestRelsTable omitTracked tables (name, table) =
  suggestRelsFK omitTracked tables name constraints tracked =<< toList foreignKeys
  where
    foreignKeys = _tciForeignKeys table
    constraints = _tciUniqueConstraints table
    tracked = H.fromList $ mapMaybe (relationships (riRTable &&& riMapping)) $ Map.elems $ _tciFieldInfoMap table
    relationships f = fmap f . preview _FIRelationship

-- NOTE: This could be grouped by table instead of a list, console stakeholders are happy with this being a list.
suggestRelsResponse ::
  forall b.
  Backend b =>
  Bool ->
  HashMap (TableName b) (TableCoreInfo b) ->
  SuggestedRelationships b
suggestRelsResponse omitTracked tables =
  Relationships $
    suggestRelsTable omitTracked tables =<< Map.toList tables

-- | Helper to filter tables considered for relationships
pluck :: Eq a => Maybe [a] -> Map.HashMap a b -> Map.HashMap a b
pluck Nothing = id
pluck (Just ks) = Map.mapMaybeWithKey (\k v -> if k `elem` ks then Just v else Nothing)

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
    Just tableCache -> pure $ encJFromJValue $ suggestRelsResponse @b omitExistingB (pluck tablesM tableCache)
