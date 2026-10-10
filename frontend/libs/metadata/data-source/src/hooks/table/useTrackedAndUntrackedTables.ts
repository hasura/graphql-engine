import { useTrackableTables } from './useTrackableTables';
import { splitByTracked } from '../../utils';
import { Source } from '@hasura/shared/types';

type UseTrackedAndUntrackedTablesProps = {
  source: Source;
  schema?: string;
};

export function useTrackedAndUntrackedTables({
  source,
  schema,
}: UseTrackedAndUntrackedTablesProps) {
  const {
    isLoading: isIntroLoading,
    isError: isIntrospectionError,
    error: introspectionError,
    data: trackableTables,
  } = useTrackableTables({
    source,
  });

  const metadataTables = source?.tables ?? [];

  // if this is not memoized it re-runs each render
  const selected = splitByTracked({
    metadataTables,
    introspectedTables: trackableTables ?? [],
    schemaFilter: schema,
  });

  return {
    ...selected,
    isIntroLoading,
    isIntrospectionError,
    introspectionError,
  };
}
