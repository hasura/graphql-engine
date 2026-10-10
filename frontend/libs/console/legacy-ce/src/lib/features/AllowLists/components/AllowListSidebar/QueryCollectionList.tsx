import { Skeleton } from '@radix-ui/themes';
import { useQueryCollections } from '../../../QueryCollections/hooks/useQueryCollections';
import { QueryCollectionItem } from './QueryCollectionItem';
import { Text } from '@hasura/shared/ui';

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
      <Skeleton className="w-full" height="20px">
        Loading...
      </Skeleton>
    );
  }

  const matchingQueryCollections = (queryCollections || []).filter(
    ({ name }) =>
      !search || name?.toLowerCase().includes(search?.toLowerCase()),
  );

  if (
    search &&
    queryCollections.length > 0 &&
    matchingQueryCollections.length === 0
  ) {
    return (
      <div>
        <Text color="gray">No results found</Text>
      </div>
    );
  }

  return (
    <div className="mb-2">
      <div>
        <Text weight="bold" className="tracking-wider uppercase">
          Collections
        </Text>
      </div>
      <div>
        {queryCollections &&
          matchingQueryCollections.map(({ name }) => (
            <QueryCollectionItem
              to={buildHref(name)}
              onClick={(e) => {
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
