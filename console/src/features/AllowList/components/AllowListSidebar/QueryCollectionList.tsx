import React from 'react';
import Skeleton from 'react-loading-skeleton';
import { useQueryCollections } from '../../../QueryCollections/hooks/useQueryCollections';
import { QueryCollectionItem } from './QueryCollectionItem';

interface QueryCollectionItemProps {
  selectedCollectionQuery: string;
  search: string;
}

export const QueryCollectionList = ({
  selectedCollectionQuery,
  search,
}: QueryCollectionItemProps) => {
  const { data: queryCollections, isLoading, isError } = useQueryCollections();

  if (isError) {
    return null; // TOOD: we're waiting for error state design
  }

  if (isLoading) {
    return <Skeleton width={200} height={20} />;
  }

  const matchingQueryCollections = (queryCollections || []).filter(
    ({ name }) => !search || name.includes(search)
  );

  return (
    <div className="-mt-2 mb-xs">
      {queryCollections &&
        matchingQueryCollections.map(({ name }) => (
          <QueryCollectionItem
            href="#"
            key={name}
            name={name}
            selected={name === selectedCollectionQuery}
          />
        ))}
    </div>
  );
};
