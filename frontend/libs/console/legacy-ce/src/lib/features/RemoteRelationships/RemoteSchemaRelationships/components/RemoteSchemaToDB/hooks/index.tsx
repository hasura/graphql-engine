import { useInconsistentMetadata } from '@hasura/metadata/api';
import { isObject, getTableLabel } from '@hasura/shared/utils';
import { useAllDriverCapabilities } from '@hasura/metadata/data-source';
import { ReactSelectOptionType } from '@hasura/shared/ui';
import { Metadata } from '@hasura/shared/types';
import { useMemo } from 'react';

export const useSourceOptions = (meta: Metadata) => {
  const {
    data: inconsistentSources = [],
    isFetching: isFetchingMetadata,
    ...rest
  } = useInconsistentMetadata((m) => {
    return m.inconsistent_objects
      .filter((item) => 'type' in item && item.type === 'source')
      .map((source) => source.definition);
  });

  const { data: driverCapabilities = [], isFetching: isFetchingDriver } =
    useAllDriverCapabilities({
      select: (data) => {
        const result = data.map((item) => {
          if (!item.capabilities)
            return {
              driver: item.driver,
              capabilities: {
                isRemoteSchemaRelationshipSupported: false,
              },
            };
          return {
            driver: item.driver,
            capabilities: {
              isRemoteSchemaRelationshipSupported: isObject(
                item.capabilities.queries?.foreach,
              ),
            },
          };
        });

        return result;
      },
    });

  const tables = useMemo(() => {
    return meta.metadata.sources
      .filter((source) => !inconsistentSources.includes(source.name))
      .filter(
        (source) =>
          driverCapabilities?.find((c) => c.driver === source?.kind)
            ?.capabilities.isRemoteSchemaRelationshipSupported,
      )
      .map((source) => {
        return source.tables.map<ReactSelectOptionType>((t) => ({
          value: {
            type: 'table',
            dataSourceName: source.name,
            table: t.table,
          },
          label: getTableLabel({
            dataSourceName: source.name,
            table: t.table,
          }),
        }));
      })
      .flat();
  }, [inconsistentSources, meta, inconsistentSources, driverCapabilities]);

  return {
    data: tables,
    isFetching: isFetchingMetadata || isFetchingDriver,
    ...rest,
  };
};
