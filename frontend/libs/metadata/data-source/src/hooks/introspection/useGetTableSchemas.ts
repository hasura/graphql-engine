import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useQuery, UseQueryOptions } from '@tanstack/react-query';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource } from '@hasura/shared/types';
import { getDatabaseMethods, NotImplementedError } from '../../driver';
import { HttpError } from '@hasura/shared/types';
import { defaultQueryOptions } from '@hasura/metadata/api';

type Options<T = string[]> = Omit<
  UseQueryOptions<string[], HttpError, T>,
  'queryKey'
>;

export const useGetTableSchemas = <T = string[]>(
  {
    source,
  }: {
    source: QualifiedDataSource | undefined;
  },
  options?: Options<T>,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery({
    queryKey: [source?.name, 'database-schemas'],
    queryFn: async () => {
      if (!source) {
        return [];
      }

      const dataSource = getDatabaseMethods(source.kind);
      if (!dataSource.introspection.getDatabaseSchemas) {
        throw new NotImplementedError(
          `getDatabaseSchemas not implemented for source: ${source.name}`,
        );
      }

      return dataSource.introspection.getDatabaseSchemas({
        endpoints,
        fetchJson,
        dataSourceName: source.name,
      });
    },
    ...defaultQueryOptions,
    ...options,
    enabled: Boolean(source) && options?.enabled !== false,
  });
};
