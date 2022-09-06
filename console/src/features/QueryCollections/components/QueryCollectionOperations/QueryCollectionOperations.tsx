import React from 'react';
import Skeleton from 'react-loading-skeleton';

import { Button } from '@/new-components/Button';
import { CardedTable } from '@/new-components/CardedTable';
import { QueryCollection } from '@/metadata/types';
import { getConfirmation } from '@/components/Common/utils/jsUtils';
import { IndicatorCard } from '@/new-components/IndicatorCard';

import { useOperationsFromQueryCollection } from '../../hooks/useOperationsFromQueryCollection';
import { useRemoveOperationsFromQueryCollection } from '../../hooks';

import { QueryCollectionsOperationsHeader } from './QueryCollectionOperationsHeader';

const Check: React.FC<React.ComponentProps<'input'>> = props => (
  <input
    type="checkbox"
    className="cursor-pointer rounded border shadow-sm border-gray-400 hover:border-gray-500 focus:ring-yellow-400"
    {...props}
  />
);

interface QueryCollectionsOperationsProps {
  collectionName: string;
  onEdit: (operation: string) => void;
}

export const QueryCollectionsOperations: React.FC<QueryCollectionsOperationsProps> =
  props => {
    const { collectionName, onEdit } = props;

    const {
      data: operations,
      isLoading,
      isError,
    } = useOperationsFromQueryCollection(collectionName);

    const { removeOperationsFromQueryCollection, isLoading: deleteLoading } =
      useRemoveOperationsFromQueryCollection();

    const [search, setSearch] = React.useState('');

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
        <IndicatorCard status="negative">
          Failed to load operations
        </IndicatorCard>
      );
    }

    const filteredOperations = (operations || []).filter(operation =>
      operation.name.toLowerCase().includes(search)
    );

    const data = filteredOperations?.map(operation => [
      <Check
        data-testid={`operation-${operation.name}`}
        checked={selectedOperations.includes(operation)}
        onClick={() => {
          setSelectedOperations(
            selectedOperations.includes(operation)
              ? selectedOperations.filter(o => o !== operation)
              : [...selectedOperations, operation]
          );
        }}
      />,
      operation.name,
      operation.query.toLowerCase().includes('mutation') ? 'Mutation' : 'Query',

      <div className="flex justify-end">
        <Button
          className="mr-1.5"
          size="sm"
          onClick={() => onEdit(operation.name)}
        >
          Edit
        </Button>

        <Button
          onClick={() => {
            const confirmMessage = `This will permanently delete the operation "${operation.name}"`;
            const isOk = getConfirmation(confirmMessage, true, operation.name);
            if (isOk) {
              removeOperationsFromQueryCollection(collectionName, [operation]);
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
        <QueryCollectionsOperationsHeader
          selectedOperations={selectedOperations}
          onSearch={searchString => {
            setSearch(searchString);
            setSelectedOperations([]);
          }}
          collectionName={collectionName}
        />
        <div>
          <CardedTable
            columns={[
              <Check
                data-testid="query-collections-select-all"
                checked={
                  selectedOperations.length === filteredOperations.length
                }
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
        </div>
      </div>
    );
  };
