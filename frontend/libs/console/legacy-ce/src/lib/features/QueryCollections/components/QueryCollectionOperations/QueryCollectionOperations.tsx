import React from 'react';
import Skeleton from 'react-loading-skeleton';

import { Button } from '../../../../new-components/Button';
import { CardedTable } from '../../../../new-components/CardedTable';
import { QueryCollection } from '../../../../metadata/types';
import { getConfirmation } from '../../../../components/Common/utils/jsUtils';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { useFireNotification } from '../../../../new-components/Notifications';

import { useOperationsFromQueryCollection } from '../../hooks/useOperationsFromQueryCollection';
import { useRemoveOperationsFromQueryCollection } from '../../hooks';

import { QueryCollectionsOperationsHeader } from './QueryCollectionOperationsHeader';
import { QueryCollectionOperationsEmptyState } from './QueryCollectionOperationsEmptyState';
import { QueryCollectionOperationEdit } from '../QueryCollectionOperationDialog/QueryCollectionOperationEdit';

const Check: React.FC<React.ComponentProps<'input'>> = props => (
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
> = props => {
  const { collectionName } = props;

  const {
    data: operations,
    isLoading,
    isError,
  } = useOperationsFromQueryCollection(collectionName);

  const { removeOperationsFromQueryCollection, isLoading: deleteLoading } =
    useRemoveOperationsFromQueryCollection();

  const { fireNotification } = useFireNotification();

  const [search, setSearch] = React.useState('');

  const [editingOperation, setEditingOperation] =
    React.useState<QueryCollection | null>(null);

  const [selectedOperations, setSelectedOperations] = React.useState<
    QueryCollection[]
  >([]);

  if (isLoading) {
    return (
      <div data-testid="query-collection-operations-loading">
        <Skeleton height={200} />
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

  const filteredOperations = (operations || []).filter(operation =>
    operation.name?.toLowerCase().includes(search?.toLowerCase())
  );

  const data = filteredOperations?.map(operation => [
    <Check
      data-testid={`operation-${operation.name}`}
      checked={selectedOperations.includes(operation)}
      onChange={() => {
        setSelectedOperations(
          selectedOperations.includes(operation)
            ? selectedOperations.filter(o => o !== operation)
            : [...selectedOperations, operation]
        );
      }}
    />,
    operation.name,
    operation.query.toLowerCase().startsWith('mutation') ? 'Mutation' : 'Query',

    <div className="flex justify-end">
      <Button
        className="mr-1.5"
        size="sm"
        onClick={() => setEditingOperation(operation)}
      >
        Edit
      </Button>

      <Button
        onClick={() => {
          const confirmMessage = `This will permanently delete the operation "${operation.name}"`;
          const isOk = getConfirmation(confirmMessage, true, operation.name);
          if (isOk) {
            removeOperationsFromQueryCollection(collectionName, [operation], {
              onSuccess: () => {
                fireNotification({
                  type: 'success',
                  title: 'Operation deleted',
                  message: `Operation "${operation.name}" was deleted successfully`,
                });
              },
              onError: e => {
                fireNotification({
                  type: 'error',
                  title: 'Failed to delete operation',
                  message: `Failed to delete operation "${operation.name}": ${e.message}`,
                });
              },
            });
          }
        }}
        className="mr-1.5"
        size="sm"
        mode="destructive"
        isLoading={deleteLoading}
      >
        Delete
      </Button>
    </div>,
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
          <div className="text-center text-gray-500">No operations found</div>
        ) : (
          <CardedTable
            columns={[
              <Check
                data-testid="query-collections-select-all"
                checked={filteredOperations.every(operation =>
                  selectedOperations.includes(operation)
                )}
                onClick={() =>
                  setSelectedOperations(
                    selectedOperations.length === 0 ? filteredOperations : []
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
