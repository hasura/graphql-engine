import React from 'react';
import {
  Button,
  CardedTable,
  IndicatorCard,
  Text,
  Tooltip,
  hasuraToast,
} from '@hasura/shared/ui';
import { getConfirmation } from '@hasura/shared/utils';
import { QueryCollectionsOperationsHeader } from './QueryCollectionOperationsHeader';
import { QueryCollectionOperationsEmptyState } from './QueryCollectionOperationsEmptyState';
import { QueryCollectionOperationEdit } from '../QueryCollectionOperationDialog/QueryCollectionOperationEdit';
import {
  useMetadata,
  useRemoveOperationsFromQueryCollection,
} from '@hasura/metadata/api';
import { QueryCollectionQuery } from '@hasura/shared/types';
import { Flex, Skeleton } from '@radix-ui/themes';
import { MetadataSelectors } from '@hasura/metadata/helpers';

const Check: React.FC<React.ComponentProps<'input'>> = (props) => (
  <input
    type="checkbox"
    className="cursor-pointer rounded border shadow-sm border-gray-400 hover:border-gray-500 focus:ring-yellow-400"
    {...props}
  />
);

interface QueryCollectionsOperationsProps {
  collectionName: string;
}

export const QueryCollectionsOperations: React.FC<
  QueryCollectionsOperationsProps
> = (props) => {
  const { collectionName } = props;
  const { data: metadata, isFetching: isLoading, isError } = useMetadata();
  const operations =
    MetadataSelectors.getOperationsFromQueryCollection(collectionName)(
      metadata,
    );

  const { removeOperationsFromQueryCollection, isPending: deleteLoading } =
    useRemoveOperationsFromQueryCollection();

  const [search, setSearch] = React.useState('');

  const [editingOperation, setEditingOperation] =
    React.useState<QueryCollectionQuery | null>(null);

  const [selectedOperations, setSelectedOperations] = React.useState<
    QueryCollectionQuery[]
  >([]);

  if (isLoading) {
    return (
      <div data-testid="query-collection-operations-loading">
        <Skeleton height="200px" />
      </div>
    );
  }

  if (isError) {
    return (
      <IndicatorCard status="negative">Failed to load operations</IndicatorCard>
    );
  }

  if (!operations || operations.length === 0) {
    return <QueryCollectionOperationsEmptyState />;
  }

  const lowerSearch = search ? search.toLowerCase() : search;
  const filteredOperations =
    (search
      ? operations.filter((operation) =>
          operation.name.toLowerCase().includes(lowerSearch),
        )
      : operations) ?? [];

  const data = filteredOperations.map((operation) => [
    <Check
      key={`check-${operation.name}`}
      data-testid={`operation-${operation.name}`}
      checked={selectedOperations.includes(operation)}
      onChange={() => {
        setSelectedOperations(
          selectedOperations.includes(operation)
            ? selectedOperations.filter((o) => o !== operation)
            : [...selectedOperations, operation],
        );
      }}
    />,
    operation.name,
    operation.query.toLowerCase().startsWith('mutation') ? 'Mutation' : 'Query',
    <Flex key={`actions-${operation.name}`} justify="end" gap="2">
      {metadata?.metadata?.rest_endpoints?.some(
        (e) =>
          e.name === operation.name &&
          e.definition.query.collection_name === collectionName,
      ) ? (
        <Tooltip content="This operation has a restified endpoint. You can edit it from restified endpoint list">
          <Button
            mode="default"
            size="1"
            onClick={() => setEditingOperation(operation)}
            disabled
          >
            Edit
          </Button>
        </Tooltip>
      ) : (
        <Button
          size="1"
          mode="default"
          onClick={() => setEditingOperation(operation)}
        >
          Edit
        </Button>
      )}

      <Button
        onClick={() => {
          const confirmMessage = `This will permanently delete the operation "${operation.name}" from the collection and related restified endpoint if exists.`;
          const isOk = getConfirmation(confirmMessage, true, operation.name);
          if (isOk) {
            removeOperationsFromQueryCollection(collectionName, [operation], {
              onSuccess: () => {
                hasuraToast({
                  type: 'success',
                  title: 'Operation deleted',
                  message: `Operation "${operation.name}" was deleted successfully`,
                });
              },
              onError: (e) => {
                hasuraToast({
                  type: 'error',
                  title: 'Failed to delete operation',
                  message: `Failed to delete operation "${operation.name}": ${e.message}`,
                });
              },
            });
          }
        }}
        size="1"
        mode="destructive"
        loading={deleteLoading}
      >
        Delete
      </Button>
    </Flex>,
  ]);

  return (
    <div>
      {editingOperation && (
        <QueryCollectionOperationEdit
          queryCollectionName={collectionName}
          operation={editingOperation}
          onClose={() => {
            setEditingOperation(null);
            setSelectedOperations([]);
          }}
        />
      )}
      <QueryCollectionsOperationsHeader
        selectedOperations={selectedOperations}
        onSearch={setSearch}
        setSelectedOperations={setSelectedOperations}
        collectionName={collectionName}
      />
      <div>
        {search && filteredOperations.length === 0 ? (
          <Text as="p" align="center">
            No operations found
          </Text>
        ) : (
          <CardedTable
            columns={[
              <Check
                key="select-all"
                data-testid="query-collections-select-all"
                checked={filteredOperations.every((operation) =>
                  selectedOperations.includes(operation),
                )}
                onClick={() =>
                  setSelectedOperations(
                    selectedOperations.length === 0 ? filteredOperations : [],
                  )
                }
              />,
              'Operation',
              'Type',
              '',
            ]}
            data={data}
          />
        )}
      </div>
    </div>
  );
};
