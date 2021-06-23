module Hasura.RQL.DDL.Relationship.Rename
  (runRenameRel)
where

import           Hasura.Prelude

import qualified Data.HashMap.Strict   as Map

import           Data.Text.Extended

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.RQL.DDL.Schema (renameRelationshipInMetadata)
import           Hasura.RQL.Types


renameRelP2
  :: forall b m
   . (QErrM m, CacheRM m, BackendMetadata b)
  => SourceName -> TableName b -> RelName -> RelInfo b -> m MetadataModifier
renameRelP2 source qt newRN relInfo = withNewInconsistentObjsCheck $ do
  tabInfo <- askTableCoreInfo @b source qt
  -- check for conflicts in fieldInfoMap
  case Map.lookup (fromRel newRN) $ _tciFieldInfoMap tabInfo of
    Nothing -> return ()
    Just _  ->
      throw400 AlreadyExists $ "cannot rename relationship " <> oldRN
      <<> " to " <> newRN <<> " in table " <> qt <<>
      " as a column/relationship with the name already exists"
  -- update metadata
  execWriterT $ renameRelationshipInMetadata @b source qt oldRN (riType relInfo) newRN
  where
    oldRN = riName relInfo

runRenameRel
  :: forall b m
   . (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b)
  => RenameRel b -> m EncJSON
runRenameRel (RenameRel source qt rn newRN) = do
  tabInfo <- askTableCoreInfo @b source qt
  ri <- askRelType (_tciFieldInfoMap tabInfo) rn ""
  withNewInconsistentObjsCheck $
    renameRelP2 source qt newRN ri >>= buildSchemaCache
  pure successMsg
