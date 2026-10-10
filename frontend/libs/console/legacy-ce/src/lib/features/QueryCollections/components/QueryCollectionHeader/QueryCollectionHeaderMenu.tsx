import { getConfirmation } from '@hasura/shared/utils';
import { useAddToAllowList, useRemoveFromAllowList } from '../../../AllowLists';
import { Button, DropdownMenu, Tooltip, hasuraToast } from '@hasura/shared/ui';
import React from 'react';
import { FaEllipsisH } from 'react-icons/fa';
import { useDeleteQueryCollections, useMetadata } from '@hasura/metadata/api';
import type { QueryCollection } from '@hasura/shared/types';

interface QueryCollectionHeaderMenuProps {
  queryCollection: QueryCollection;
  onDelete: (name: string) => void;
  onRename: (name: string, newName: string) => void;
  setIsRenameModalOpen: (isRenameModalOpen: boolean) => void;
}
export const QueryCollectionHeaderMenu: React.FC<
  QueryCollectionHeaderMenuProps
> = (props) => {
  const { queryCollection, onDelete, setIsRenameModalOpen } = props;
  const { deleteQueryCollection, isPending: deleteLoading } =
    useDeleteQueryCollections();
  const { addToAllowList, isLoading: addLoading } = useAddToAllowList();
  const { removeFromAllowList, isLoading: removeLoding } =
    useRemoveFromAllowList();

  const { data: metadata } = useMetadata();
  return queryCollection.name !== 'allowed-queries' ? (
    <DropdownMenu.Root
      items={[
        <DropdownMenu.Item
          key="edit-collection-name"
          onSelect={() => setIsRenameModalOpen(true)}
        >
          Edit Collection Name
        </DropdownMenu.Item>,
        metadata?.metadata.allowlist?.find(
          (entry) => entry.collection === queryCollection.name,
        ) ? (
          <DropdownMenu.Item
            key="allow-list"
            onClick={() => {
              removeFromAllowList(queryCollection.name, {
                onSuccess: () => {
                  hasuraToast({
                    type: 'success',
                    title: 'Success',
                    message: `Removed ${queryCollection.name} from allow list`,
                  });
                },
                onError: () => {
                  hasuraToast({
                    type: 'error',
                    title: 'Error',
                    message: `Failed to remove ${queryCollection.name} from allow list`,
                  });
                },
              });
            }}
          >
            Remove from Allow List
          </DropdownMenu.Item>
        ) : (
          <DropdownMenu.Item
            key="allow-list"
            onClick={() => {
              addToAllowList(queryCollection.name, {
                onSuccess: () => {
                  hasuraToast({
                    type: 'success',
                    title: 'Success',
                    message: `Added ${queryCollection.name} to allow list`,
                  });
                },
                onError: () => {
                  hasuraToast({
                    type: 'error',
                    title: 'Error',
                    message: `Failed to add ${queryCollection.name} to allow list`,
                  });
                },
              });
            }}
          >
            Add to Allow List
          </DropdownMenu.Item>
        ),
        <DropdownMenu.Item
          key="delete"
          color="red"
          onClick={() => {
            const confirmMessage = `This will permanently delete the query collection "${queryCollection.name}"`;
            const isOk = getConfirmation(
              confirmMessage,
              true,
              queryCollection.name,
            );
            if (isOk) {
              deleteQueryCollection(queryCollection.name, {
                onSuccess: () => {
                  hasuraToast({
                    type: 'success',
                    title: 'Collection deleted',
                    message: `Query collection "${queryCollection.name}" deleted successfully`,
                  });
                  onDelete(queryCollection.name);
                },
                onError: () => {
                  hasuraToast({
                    type: 'error',
                    title: 'Error deleting collection',
                    message: `Error deleting query collection "${queryCollection.name}"`,
                  });
                },
              });
            }
          }}
        >
          Delete Collection
        </DropdownMenu.Item>,
      ]}
    >
      <Button loading={deleteLoading || addLoading || removeLoding}>
        <FaEllipsisH />
      </Button>
    </DropdownMenu.Root>
  ) : (
    <Tooltip content="You cannot rename or delete the default allowed-queries collection">
      <Button disabled>
        <FaEllipsisH />
      </Button>
    </Tooltip>
  );
};
