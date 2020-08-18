module Hasura.RQL.DDL.Relationship.Rename
  (runRenameRel)
where

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema (renameRelationshipInMetadata)
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Data.HashMap.Strict   as Map

renameRelP2
  :: (QErrM m, CacheRM m)
  => QualifiedTable -> RelName -> RelInfo -> m MetadataModifier
renameRelP2 qt newRN relInfo = withNewInconsistentObjsCheck $ do
  tabInfo <- askTableCoreInfo qt
  -- check for conflicts in fieldInfoMap
  case Map.lookup (fromRel newRN) $ _tciFieldInfoMap tabInfo of
    Nothing -> return ()
    Just _  ->
      throw400 AlreadyExists $ "cannot rename relationship " <> oldRN
      <<> " to " <> newRN <<> " in table " <> qt <<>
      " as a column/relationship with the name already exists"
  -- update metadata
  execWriterT $ renameRelationshipInMetadata qt oldRN (riType relInfo) newRN
  where
    oldRN = riName relInfo

runRenameRel
  :: (MonadTx m, CacheRWM m)
  => RenameRel -> m EncJSON
runRenameRel (RenameRel qt rn newRN) = do
  tabInfo <- askTableCoreInfo qt
  ri <- askRelType (_tciFieldInfoMap tabInfo) rn ""
  withNewInconsistentObjsCheck $
    renameRelP2 qt newRN ri >>= buildSchemaCache . unMetadataModifier
  pure successMsg
