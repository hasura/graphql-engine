module Hasura.RQL.DDL.Relationship.Rename
  ( RenameRel,
    runRenameRel,
  )
where

import Data.Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.DDL.Schema (renameRelationshipInMetadata)
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.Table.Cache

data RenameRel b = RenameRel
  { _rrSource :: SourceName,
    _rrTable :: TableName b,
    _rrName :: RelName,
    _rrNewName :: RelName
  }

instance (Backend b) => FromJSON (RenameRel b) where
  parseJSON = withObject "RenameRel" $ \o ->
    RenameRel
      <$> o
      .:? "source"
      .!= defaultSource
      <*> o
      .: "table"
      <*> o
      .: "name"
      <*> o
      .: "new_name"

renameRelP2 ::
  forall b m.
  (QErrM m, CacheRM m, BackendMetadata b) =>
  SourceName ->
  TableName b ->
  RelName ->
  RelInfo b ->
  m MetadataModifier
renameRelP2 source qt newRN relInfo = withNewInconsistentObjsCheck $ do
  tabInfo <- askTableCoreInfo @b source qt
  -- check for conflicts in fieldInfoMap
  case HashMap.lookup (fromRel newRN) $ _tciFieldInfoMap tabInfo of
    Nothing -> return ()
    Just _ ->
      throw400 AlreadyExists
        $ "cannot rename relationship "
        <> oldRN
        <<> " to "
        <> newRN
        <<> " in table "
        <> qt
        <<> " as a column/relationship with the name already exists"
  -- update metadata
  execWriterT $ renameRelationshipInMetadata @b source qt oldRN (riType relInfo) newRN
  where
    oldRN = riName relInfo

runRenameRel ::
  forall b m.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  RenameRel b ->
  m EncJSON
runRenameRel (RenameRel source qt rn newRN) = do
  tabInfo <- askTableCoreInfo @b source qt
  ri <- askRelType (_tciFieldInfoMap tabInfo) rn ""
  withNewInconsistentObjsCheck
    $ renameRelP2 source qt newRN ri
    >>= buildSchemaCache
  pure successMsg
