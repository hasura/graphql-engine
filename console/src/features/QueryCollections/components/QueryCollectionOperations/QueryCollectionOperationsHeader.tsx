import React from 'react';
import { FaRegCopy, FaRegFolder, FaRegTrashAlt } from 'react-icons/fa';

import { Button } from '@/new-components/Button';
import { DropdownMenu } from '@/new-components/DropdownMenu';
import { QueryCollection } from '@/metadata/types';
import { getConfirmation } from '@/components/Common/utils/jsUtils';

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
  onSearch: (search: string) => void;
}

export const QueryCollectionsOperationsHeader: React.FC<QueryCollectionsOperationsHeaderProps> =
  props => {
    const { collectionName, selectedOperations, onSearch } = props;
    const { data: queryCollections } = useQueryCollections();

    const { addOperationToQueryCollection, isLoading: addLoading } =
      useAddOperationsToQueryCollection();
    const { moveOperationToQueryCollection, isLoading: moveLoading } =
      useMoveOperationsToQueryCollection();
    const { removeOperationsFromQueryCollection, isLoading: deleteLoading } =
      useRemoveOperationsFromQueryCollection();

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
              <p className="text-sm text-muted mr-1.5">
                {selectedOperations.length} Operations:
              </p>
              {otherCollections?.length > 0 && (
                <>
                  <DropdownMenu
                    items={[
                      otherCollections.map(collection => (
                        <div
                          onClick={() =>
                            moveOperationToQueryCollection(
                              collectionName,
                              collection.name,
                              selectedOperations,
                              {
                                onError: () => {
                                  // TODO: global notifications
                                },
                                onSuccess: () => {
                                  // TODO: global notifications
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
                          data-testid={`add-to-${collection.name}`}
                          onClick={() =>
                            addOperationToQueryCollection(
                              collection.name,
                              selectedOperations,
                              {
                                onError: () => {
                                  // TODO: global notifications
                                },
                                onSuccess: () => {
                                  // TODO: global notifications
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
                        onError: () => {
                          // TODO: global notifications
                        },
                        onSuccess: () => {
                          // TODO: global notifications
                        },
                      }
                    );
                  }
                }}
                className="mr-1.5"
                size="sm"
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
          <QueryCollectionsOperationsSearchForm setSearch={onSearch} />
        </div>
      </div>
    );
  };
