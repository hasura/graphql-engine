import React from 'react';
import { FaRegCopy, FaRegFolder, FaRegTrashAlt } from 'react-icons/fa';
import { Button, DropdownMenu, hasuraToast, Text } from '@hasura/shared/ui';
import { getConfirmation } from '@hasura/shared/utils';
import { QueryCollectionsOperationsSearchForm } from './QueryCollectionOperationsSearchForm';
import { useQueryCollections } from '../../hooks/useQueryCollections';
import { QueryCollectionQuery } from '@hasura/shared/types';
import { Flex } from '@radix-ui/themes';
import {
  useAddOperationsToQueryCollection,
  useMoveOperationsToQueryCollection,
  useRemoveOperationsFromQueryCollection,
} from '@hasura/metadata/api';

interface QueryCollectionsOperationsHeaderProps {
  collectionName: string;
  selectedOperations: QueryCollectionQuery[];
  setSelectedOperations: (operations: QueryCollectionQuery[]) => void;
  onSearch: (search: string) => void;
}

export const QueryCollectionsOperationsHeader: React.FC<
  QueryCollectionsOperationsHeaderProps
> = (props) => {
  const {
    collectionName,
    selectedOperations,
    onSearch,
    setSelectedOperations,
  } = props;
  const { data: queryCollections } = useQueryCollections();

  const { addOperationToQueryCollection, isPending: addLoading } =
    useAddOperationsToQueryCollection();
  const { moveOperationToQueryCollection, isPending: moveLoading } =
    useMoveOperationsToQueryCollection();
  const { removeOperationsFromQueryCollection, isPending: deleteLoading } =
    useRemoveOperationsFromQueryCollection();

  const otherCollections = (queryCollections || []).filter(
    (c) => c.name !== collectionName,
  );

  return (
    <Flex align="center" className="mb-1">
      <Flex align="center">
        {selectedOperations.length > 0 && (
          <Flex
            align="center"
            gap="2"
            data-testid="selected-operations-controls"
          >
            <Text>{selectedOperations.length} Operations:</Text>
            {otherCollections?.length > 0 && (
              <>
                <DropdownMenu.Root
                  items={otherCollections.map((collection) => (
                    <DropdownMenu.Item
                      key={collection.name}
                      onSelect={() =>
                        moveOperationToQueryCollection(
                          collectionName,
                          collection.name,
                          selectedOperations,
                          {
                            onError: (e) => {
                              hasuraToast({
                                type: 'error',
                                title: 'Failed to move operations',
                                message: `Failed to move operations to collection ${collection.name}: ${e.message}`,
                              });
                            },
                            onSuccess: () => {
                              hasuraToast({
                                type: 'success',
                                title: 'Operations moved',
                                message: `Successfully moved ${selectedOperations.length} operations to ${collection.name}`,
                              });
                              setSelectedOperations([]);
                            },
                          },
                        )
                      }
                    >
                      {collection.name}
                    </DropdownMenu.Item>
                  ))}
                >
                  <Button
                    size="1"
                    leftIcon={FaRegFolder}
                    loading={moveLoading}
                    loadingText="Moving..."
                  >
                    Move
                  </Button>
                </DropdownMenu.Root>
                <DropdownMenu.Root
                  items={otherCollections.map((collection) => (
                    <DropdownMenu.Item
                      key={collection.name}
                      onSelect={() =>
                        addOperationToQueryCollection(
                          collection.name,
                          selectedOperations,
                          {
                            onError: (e) => {
                              hasuraToast({
                                type: 'error',
                                title: 'Failed to add operations',
                                message: `Failed to add operations to collection ${collection.name}: ${e.message}`,
                              });
                            },
                            onSuccess: () => {
                              hasuraToast({
                                type: 'success',
                                title: 'Operations added',
                                message: `Successfully added ${selectedOperations.length} operations to ${collection.name}`,
                              });
                            },
                          },
                        )
                      }
                    >
                      {collection.name}
                    </DropdownMenu.Item>
                  ))}
                >
                  <Button size="1" leftIcon={FaRegCopy} loading={addLoading}>
                    Copy
                  </Button>
                </DropdownMenu.Root>
              </>
            )}
            <Button
              onClick={() => {
                const confirmMessage = `This will permanently delete the selected operations and related restified endpoints if exists.`;
                const isOk = getConfirmation(confirmMessage, true, 'delete');
                if (isOk) {
                  removeOperationsFromQueryCollection(
                    collectionName,
                    selectedOperations,
                    {
                      onError: (e) => {
                        hasuraToast({
                          type: 'error',
                          title: 'Failed to delete operations',
                          message: `Failed to delete operations from collection ${collectionName}: ${e.message}`,
                        });
                      },
                      onSuccess: () => {
                        hasuraToast({
                          type: 'success',
                          title: 'Operations deleted',
                          message: `Successfully deleted ${selectedOperations.length} operations from ${collectionName}`,
                        });
                        setSelectedOperations([]);
                      },
                    },
                  );
                }
              }}
              size="1"
              mode="destructive"
              leftIcon={FaRegTrashAlt}
              loading={deleteLoading}
              loadingText="Deleting..."
            >
              Delete
            </Button>
          </Flex>
        )}
      </Flex>
      <div className="ml-auto w-3/12 relative">
        <QueryCollectionsOperationsSearchForm
          setSearch={(searchString) => {
            onSearch(searchString);
            setSelectedOperations([]);
          }}
        />
      </div>
    </Flex>
  );
};
