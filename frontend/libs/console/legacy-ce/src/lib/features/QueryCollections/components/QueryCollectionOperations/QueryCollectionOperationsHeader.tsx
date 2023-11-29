import React from 'react';
import { FaRegCopy, FaRegFolder, FaRegTrashAlt } from 'react-icons/fa';

import { Button } from '../../../../new-components/Button';
import { DropdownMenu } from '../../../../new-components/DropdownMenu';
import { QueryCollection } from '../../../../metadata/types';
import { getConfirmation } from '../../../../components/Common/utils/jsUtils';
import { useFireNotification } from '../../../../new-components/Notifications';

import { QueryCollectionsOperationsSearchForm } from './QueryCollectionOperationsSearchForm';
import { useQueryCollections } from '../../hooks/useQueryCollections';
import {
  useAddOperationsToQueryCollection,
  useMoveOperationsToQueryCollection,
  useRemoveOperationsFromQueryCollection,
} from '../../hooks';

interface QueryCollectionsOperationsHeaderProps {
  collectionName: string;
  selectedOperations: QueryCollection[];
  setSelectedOperations: (operations: QueryCollection[]) => void;
  onSearch: (search: string) => void;
}

export const QueryCollectionsOperationsHeader: React.FC<
  QueryCollectionsOperationsHeaderProps
> = props => {
  const {
    collectionName,
    selectedOperations,
    onSearch,
    setSelectedOperations,
  } = props;
  const { data: queryCollections } = useQueryCollections();

  const { addOperationToQueryCollection, isLoading: addLoading } =
    useAddOperationsToQueryCollection();
  const { moveOperationToQueryCollection, isLoading: moveLoading } =
    useMoveOperationsToQueryCollection();
  const { removeOperationsFromQueryCollection, isLoading: deleteLoading } =
    useRemoveOperationsFromQueryCollection();

  const { fireNotification } = useFireNotification();

  const otherCollections = (queryCollections || []).filter(
    c => c.name !== collectionName
  );

  return (
    <div className="flex items-center mb-xs">
      <div className="flex items-center">
        {selectedOperations.length > 0 && (
          <div
            className="flex items-center"
            data-testid="selected-operations-controls"
          >
            <span className="text-sm text-muted mr-1.5">
              {selectedOperations.length} Operations:
            </span>
            {otherCollections?.length > 0 && (
              <>
                <DropdownMenu
                  items={[
                    otherCollections.map(collection => (
                      <div
                        className="py-xs"
                        onClick={() =>
                          moveOperationToQueryCollection(
                            collectionName,
                            collection.name,
                            selectedOperations,
                            {
                              onError: e => {
                                fireNotification({
                                  type: 'error',
                                  title: 'Failed to move operations',
                                  message: `Failed to move operations to collection ${collection.name}: ${e.message}`,
                                });
                              },
                              onSuccess: () => {
                                fireNotification({
                                  type: 'success',
                                  title: 'Operations moved',
                                  message: `Successfully moved ${selectedOperations.length} operations to ${collection.name}`,
                                });
                                setSelectedOperations([]);
                              },
                            }
                          )
                        }
                      >
                        {collection.name}
                      </div>
                    )),
                  ]}
                >
                  <Button
                    className="mr-1.5"
                    size="sm"
                    icon={<FaRegFolder />}
                    isLoading={moveLoading}
                  >
                    Move
                  </Button>
                </DropdownMenu>
                <DropdownMenu
                  items={[
                    otherCollections.map(collection => (
                      <div
                        className="py-xs"
                        data-testid={`add-to-${collection.name}`}
                        onClick={() =>
                          addOperationToQueryCollection(
                            collection.name,
                            selectedOperations,
                            {
                              onError: e => {
                                fireNotification({
                                  type: 'error',
                                  title: 'Failed to add operations',
                                  message: `Failed to add operations to collection ${collection.name}: ${e.message}`,
                                });
                              },
                              onSuccess: () => {
                                fireNotification({
                                  type: 'success',
                                  title: 'Operations added',
                                  message: `Successfully added ${selectedOperations.length} operations to ${collection.name}`,
                                });
                              },
                            }
                          )
                        }
                      >
                        {collection.name}
                      </div>
                    )),
                  ]}
                >
                  <Button
                    className="mr-1.5"
                    size="md"
                    icon={<FaRegCopy />}
                    isLoading={addLoading}
                  >
                    Copy
                  </Button>
                </DropdownMenu>
              </>
            )}
            <Button
              onClick={() => {
                const confirmMessage = `This will permanently delete the selected operations"`;
                const isOk = getConfirmation(confirmMessage, true, 'delete');
                if (isOk) {
                  removeOperationsFromQueryCollection(
                    collectionName,
                    selectedOperations,
                    {
                      onError: e => {
                        fireNotification({
                          type: 'error',
                          title: 'Failed to delete operations',
                          message: `Failed to delete operations from collection ${collectionName}: ${e.message}`,
                        });
                      },
                      onSuccess: () => {
                        fireNotification({
                          type: 'success',
                          title: 'Operations deleted',
                          message: `Successfully deleted ${selectedOperations.length} operations from ${collectionName}`,
                        });
                        setSelectedOperations([]);
                      },
                    }
                  );
                }
              }}
              className="mr-1.5"
              size="md"
              mode="destructive"
              icon={<FaRegTrashAlt />}
              isLoading={deleteLoading}
            >
              Delete
            </Button>
          </div>
        )}
      </div>
      <div className="ml-auto w-3/12 relative">
        <QueryCollectionsOperationsSearchForm
          setSearch={searchString => {
            onSearch(searchString);
            setSelectedOperations([]);
          }}
        />
      </div>
    </div>
  );
};
