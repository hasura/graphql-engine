module Hasura.RQL.DDL.Relationship.Rename
  (runRenameRel)
where

import           Data.Text.Extended
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema              (renameRelationshipInMetadata)
import           Hasura.RQL.Types

import qualified Data.HashMap.Strict                as Map

renameRelP2
  :: (QErrM m, CacheRM m)
  => SourceName -> QualifiedTable -> RelName -> RelInfo 'Postgres -> m MetadataModifier
renameRelP2 source qt newRN relInfo = withNewInconsistentObjsCheck $ do
  tabInfo <- askTableCoreInfo source qt
  -- check for conflicts in fieldInfoMap
  case Map.lookup (fromRel newRN) $ _tciFieldInfoMap tabInfo of
    Nothing -> return ()
    Just _  ->
      throw400 AlreadyExists $ "cannot rename relationship " <> oldRN
      <<> " to " <> newRN <<> " in table " <> qt <<>
      " as a column/relationship with the name already exists"
  -- update metadata
  execWriterT $ renameRelationshipInMetadata source qt oldRN (riType relInfo) newRN
  where
    oldRN = riName relInfo

runRenameRel
  :: (MonadError QErr m, CacheRWM m, MetadataM m)
  => RenameRel -> m EncJSON
runRenameRel (RenameRel source qt rn newRN) = do
  tabInfo <- askTableCoreInfo source qt
  ri <- askRelType (_tciFieldInfoMap tabInfo) rn ""
  withNewInconsistentObjsCheck $
    renameRelP2 source qt newRN ri >>= buildSchemaCache
  pure successMsg
