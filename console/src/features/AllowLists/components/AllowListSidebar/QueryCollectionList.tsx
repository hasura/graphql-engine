import React from 'react';
import Skeleton from 'react-loading-skeleton';
import { useQueryCollections } from '../../../QueryCollections/hooks/useQueryCollections';
import { QueryCollectionItem } from './QueryCollectionItem';

interface QueryCollectionItemProps {
  selectedCollectionQuery: string;
  search: string;
  buildHref: (name: string) => string;
  onClick: (url: string) => void;
}

export const QueryCollectionList = (props: QueryCollectionItemProps) => {
  const { selectedCollectionQuery, search, buildHref, onClick } = props;

  const { data: queryCollections, isLoading, isError } = useQueryCollections();

  if (isError) {
    return null; // TOOD: we're waiting for error state design
  }

  if (isLoading) {
    return (
      <div className="px-sm -mt-2 mb-xs">
        <Skeleton width={200} height={20} />
      </div>
    );
  }

  const matchingQueryCollections = (queryCollections || []).filter(
    ({ name }) => !search || name?.toLowerCase().includes(search?.toLowerCase())
  );

  if (
    search &&
    queryCollections.length > 0 &&
    matchingQueryCollections.length === 0
  ) {
    return (
      <div className="px-sm -mt-2 mb-xs">
        <p className="text-gray-500">No results found</p>
      </div>
    );
  }

  return (
    <div>
      <div className="px-sm -ml-3 mb-xs">
        <p className="text-sm font-semibold text-muted uppercase tracking-wider uppercase">
          Collections
        </p>
      </div>
      <div className="-mt-2 mb-xs">
        {queryCollections &&
          matchingQueryCollections.map(({ name }) => (
            <QueryCollectionItem
              href={buildHref(name)}
              onClick={e => {
                onClick(buildHref(name));
                e.preventDefault();
              }}
              key={name}
              name={name}
              selected={name === selectedCollectionQuery}
            />
          ))}
      </div>
    </div>
  );
};
