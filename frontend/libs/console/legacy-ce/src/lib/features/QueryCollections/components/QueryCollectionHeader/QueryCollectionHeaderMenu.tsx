import { getConfirmation } from '../../../../components/Common/utils/jsUtils';
import { useAddToAllowList, useRemoveFromAllowList } from '../../../AllowLists';
import { useMetadata } from '../../../MetadataAPI';
import { QueryCollectionEntry } from '../../../../metadata/types';
import { Button } from '../../../../new-components/Button';
import { DropdownMenu } from '../../../../new-components/DropdownMenu';
import { Tooltip } from '../../../../new-components/Tooltip';
import { useFireNotification } from '../../../../new-components/Notifications';
import React from 'react';
import { FaEllipsisH } from 'react-icons/fa';
import { useDeleteQueryCollections } from '../../hooks/useDeleteQueryCollections';

interface QueryCollectionHeaderMenuProps {
  queryCollection: QueryCollectionEntry;
  onDelete: (name: string) => void;
  onRename: (name: string, newName: string) => void;
  setIsRenameModalOpen: (isRenameModalOpen: boolean) => void;
}
export const QueryCollectionHeaderMenu: React.FC<
  QueryCollectionHeaderMenuProps
> = props => {
  const { queryCollection, onDelete, setIsRenameModalOpen } = props;

  const { deleteQueryCollection, isLoading: deleteLoading } =
    useDeleteQueryCollections();
  const { fireNotification } = useFireNotification();

  const { addToAllowList, isLoading: addLoading } = useAddToAllowList();
  const { removeFromAllowList, isLoading: removeLoding } =
    useRemoveFromAllowList();

  const { data: metadata } = useMetadata();
  return queryCollection.name !== 'allowed-queries' ? (
    <DropdownMenu
      items={[
        [
          <div
            className="py-xs font-semibold"
            onClick={() => {
              // this is a workaround for a weird but caused by interaction of radix ui dialog and dropdown menu
              setTimeout(() => {
                setIsRenameModalOpen(true);
              }, 0);
            }}
          >
            Edit Collection Name
          </div>,
          metadata?.metadata.allowlist?.find(
            entry => entry.collection === queryCollection.name
          ) ? (
            <div
              className="py-xs font-semibold"
              onClick={() => {
                removeFromAllowList(queryCollection.name, {
                  onSuccess: () => {
                    fireNotification({
                      type: 'success',
                      title: 'Success',
                      message: `Removed ${queryCollection.name} from allow list`,
                    });
                  },
                  onError: () => {
                    fireNotification({
                      type: 'error',
                      title: 'Error',
                      message: `Failed to remove ${queryCollection.name} from allow list`,
                    });
                  },
                });
              }}
            >
              Remove from Allow List
            </div>
          ) : (
            <div
              className="font-semibold"
              onClick={() => {
                addToAllowList(queryCollection.name, {
                  onSuccess: () => {
                    fireNotification({
                      type: 'success',
                      title: 'Success',
                      message: `Added ${queryCollection.name} to allow list`,
                    });
                  },
                  onError: () => {
                    fireNotification({
                      type: 'error',
                      title: 'Error',
                      message: `Failed to add ${queryCollection.name} to allow list`,
                    });
                  },
                });
              }}
            >
              Add to Allow List
            </div>
          ),
        ],
        [
          <div
            className="py-xs font-semibold text-red-600"
            onClick={() => {
              const confirmMessage = `This will permanently delete the query collection "${queryCollection.name}"`;
              const isOk = getConfirmation(
                confirmMessage,
                true,
                queryCollection.name
              );
              if (isOk) {
                deleteQueryCollection(queryCollection.name, {
                  onSuccess: () => {
                    fireNotification({
                      type: 'success',
                      title: 'Collection deleted',
                      message: `Query collection "${queryCollection.name}" deleted successfully`,
                    });
                    onDelete(queryCollection.name);
                  },
                  onError: () => {
                    fireNotification({
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
          </div>,
        ],
      ]}
    >
      <Button isLoading={deleteLoading || addLoading || removeLoding}>
        <FaEllipsisH />
      </Button>
    </DropdownMenu>
  ) : (
    <Tooltip
      tooltipContentChildren="You cannot rename or delete the default allowed-queries
              collection"
    >
      <Button disabled>
        <FaEllipsisH />
      </Button>
    </Tooltip>
  );
};
