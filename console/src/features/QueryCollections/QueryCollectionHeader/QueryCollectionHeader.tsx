import { getConfirmation } from '@/components/Common/utils/jsUtils';
import {
  useAddToAllowList,
  useRemoveFromAllowList,
} from '@/features/AllowLists';
import { useMetadata } from '@/features/MetadataAPI';
import { QueryCollectionEntry } from '@/metadata/types';
import { Button } from '@/new-components/Button';
import { DropdownMenu } from '@/new-components/DropdownMenu';
import React, { useState } from 'react';
import { FaEllipsisH, FaPlusCircle } from 'react-icons/fa';
import { useDeleteQueryCollections } from '../hooks/useDeleteQueryCollections';
import { QueryCollectionRenameDialog } from './QueryCollectionRenameDialog';

interface QueryCollectionHeaderProps {
  queryCollection: QueryCollectionEntry;
}
export const QueryCollectionHeader: React.FC<QueryCollectionHeaderProps> =
  props => {
    const { queryCollection } = props;

    const [isRenameModalOpen, setIsRenameModalOpen] = useState(false);
    const { deleteQueryCollection, isLoading: deleteLoading } =
      useDeleteQueryCollections();
    const { addToAllowList, isLoading: addLoading } = useAddToAllowList();
    const { removeFromAllowList, isLoading: removeLoding } =
      useRemoveFromAllowList();

    const { data: metadata } = useMetadata();
    return (
      <>
        {isRenameModalOpen && (
          <QueryCollectionRenameDialog
            currentName={queryCollection.name}
            onClose={() => {
              setIsRenameModalOpen(false);
            }}
          />
        )}
        <div className="flex items-center mb-xs">
          <div>
            <h1 className="text-xl font-semibold">{queryCollection.name}</h1>
            <p className="text-muted">
              Add queries to the collection to create a safe list of operations
              which can be run against your GraphQL API.
            </p>
          </div>
          <div className="relative ml-auto mr-sm">
            <DropdownMenu
              items={[
                [
                  <div
                    className="font-semibold"
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
                      className="font-semibold"
                      onClick={() => {
                        removeFromAllowList(queryCollection.name, {
                          onSuccess: () => {
                            // fire notification
                          },
                          onError: () => {
                            // fire notification
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
                            // fire notification
                          },
                          onError: () => {
                            // fire notification
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
                    className="font-semibold text-red-600"
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
                            // fire notification
                          },
                          onError: () => {
                            // fire notification
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
          </div>
          <Button mode="primary" icon={<FaPlusCircle />}>
            Add Operation
          </Button>
        </div>
      </>
    );
  };
