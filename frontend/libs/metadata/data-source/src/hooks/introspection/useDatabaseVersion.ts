import { useQuery } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { useMetadata } from '@hasura/metadata/api';
import { getDatabaseMethods } from '../../driver';

export const useDatabaseVersion = (
  dataSourceNames: string[],
  enabled?: boolean,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const { data: meta } = useMetadata();

  return useQuery({
    queryKey: ['dbVersion', ...dataSourceNames],
    queryFn: async () => {
      const result = [...new Set(dataSourceNames)].map(
        async (dataSourceName) => {
          const source = meta?.metadata.sources.find(
            (s) => s.name === dataSourceName,
          );
          if (!source) {
            return {
              dataSourceName,
            };
          }

          const databaseMethods = getDatabaseMethods(source.kind);
          if (!databaseMethods.introspection.getVersion) {
            return {
              dataSourceName,
            };
          }

          try {
            const version = await databaseMethods.introspection.getVersion({
              dataSourceName,
              fetchJson,
              endpoints,
            });

            return {
              dataSourceName,
              version,
            };
          } catch (err) {
            return {
              dataSourceName,
            };
          }
        },
      );

      return Promise.all(result);
    },
    enabled: enabled,
  });
};
