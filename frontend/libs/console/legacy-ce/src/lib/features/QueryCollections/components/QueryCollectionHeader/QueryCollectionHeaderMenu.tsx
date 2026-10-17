import { useAddToAllowList, useRemoveFromAllowList } from '../../../AllowLists';
import {
  Button,
  DestructiveDialogCascade,
  DropdownMenu,
  IconTooltip,
  getDestructiveDescription,
  hasuraToast,
} from '@hasura/shared/ui';
import React, { useState } from 'react';
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
  const { mutate: deleteQueryCollection, isPending: deleteLoading } =
    useDeleteQueryCollections();
  const [isDeleteDialogOpen, setIsDeleteDialogOpen] = useState(false);
  const { addToAllowList, isLoading: addLoading } = useAddToAllowList();
  const { removeFromAllowList, isLoading: removeLoding } =
    useRemoveFromAllowList();

  const { data: metadata } = useMetadata();

  const onDeleteConfirm = (cascade: boolean) =>
    deleteQueryCollection(
      { collection: queryCollection.name, cascade },
      {
        onSuccess: () => {
          setIsDeleteDialogOpen(false);
          onDelete(queryCollection.name);
        },
      },
    );

  return queryCollection.name !== 'allowed-queries' ? (
    <>
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
            onClick={() => setIsDeleteDialogOpen(true)}
          >
            Delete Collection
          </DropdownMenu.Item>,
        ]}
      >
        <Button loading={deleteLoading || addLoading || removeLoding}>
          <FaEllipsisH />
        </Button>
      </DropdownMenu.Root>
      {isDeleteDialogOpen && (
        <DestructiveDialogCascade
          title="Delete query collection"
          confirmButtonProps={{
            children: 'Delete',
            loadingText: 'Deleting...',
          }}
          onClose={() => setIsDeleteDialogOpen(false)}
          onConfirm={onDeleteConfirm}
          // Matches the previous behaviour, which always cascaded so that
          // collections in the allow list can be deleted.
          defaultCascade
          loading={deleteLoading}
        >
          {getDestructiveDescription({
            destroyTerm: 'delete',
            resourceName: queryCollection.name,
            resourceType: 'query collection',
          })}
        </DestructiveDialogCascade>
      )}
    </>
  ) : (
    <IconTooltip
      message="You cannot rename or delete the default allowed-queries collection"
      icon={<FaEllipsisH />}
    />
  );
};
