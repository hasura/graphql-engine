import {
  useInconsistentMetadata,
  useMetadata,
} from '../../hasura-metadata-api';
import { SourceOption } from '../components/RelationshipForm/parts/SourceSelect';
import { getTableLabel } from '../components/RelationshipForm/utils';

export const useSourceOptions = () => {
  const { data: inconsistentSources = [], isFetching } =
    useInconsistentMetadata(m => {
      return m.inconsistent_objects
        .filter(item => item.type === 'source')
        .map(source => source.definition);
    });

  return useMetadata(
    m => {
      const tables: SourceOption[] = m.metadata.sources
        .filter(source => !inconsistentSources.includes(source.name))
        .map(source => {
          return source.tables.map<SourceOption>(t => ({
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

      const remoteSchemas = (m.metadata.remote_schemas ?? []).map<SourceOption>(
        rs => ({
          value: { type: 'remoteSchema', remoteSchema: rs.name },
          label: rs.name,
        })
      );

      return [...tables, ...remoteSchemas];
    },
    {
      enabled: !isFetching,
    }
  );
};
