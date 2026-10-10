import { useInconsistentMetadata, useMetadata } from '@hasura/metadata/api';
import { getTableLabel } from '@hasura/shared/utils';
import { SourceOption } from '../components/RelationshipForm/parts/SourceSelect';
import { useMemo } from 'react';

export const useSourceOptions = () => {
  const { data: inconsistentSources } = useInconsistentMetadata((m) => {
    return m.inconsistent_objects
      .filter((item) => 'type' in item && item.type === 'source')
      .map((source) => source.definition);
  });

  const { data: m, ...queryResults } = useMetadata();

  const sourceOptions = useMemo(() => {
    const tables: SourceOption[] =
      m?.metadata.sources
        .filter((source) => !inconsistentSources?.includes(source.name))
        .map((source) => {
          return source.tables.map<SourceOption>((t) => ({
            value: {
              type: 'table',
              dataSourceName: source.name,
              driver: source.kind,
              table: t.table,
            },
            label: getTableLabel({
              dataSourceName: source.name,
              table: t.table,
            }),
          }));
        })
        .flat() ?? [];

    const remoteSchemas = (m?.metadata.remote_schemas ?? []).map<SourceOption>(
      (rs) => ({
        value: { type: 'remoteSchema', remoteSchema: rs.name },
        label: rs.name,
      }),
    );

    return [...tables, ...remoteSchemas];
  }, [m, inconsistentSources]);

  return {
    ...queryResults,
    sourceOptions,
    inconsistentSources: inconsistentSources ?? [],
  };
};
