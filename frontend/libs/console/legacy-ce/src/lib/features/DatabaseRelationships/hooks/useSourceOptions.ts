import { useMetadata } from '../../hasura-metadata-api';
import { SourceOption } from '../components/RelationshipForm/parts/SourceSelect';
import { getTableLabel } from '../components/RelationshipForm/utils';

export const useSourceOptions = () => {
  return useMetadata(m => {
    const tables: SourceOption[] = [
      ...m.metadata.sources.map(source => {
        return source.tables.map<SourceOption>(t => ({
          value: { type: 'table', dataSourceName: source.name, table: t.table },
          label: getTableLabel({ dataSourceName: source.name, table: t.table }),
        }));
      }),
    ].flat();

    const remoteSchemas = (m.metadata.remote_schemas ?? []).map<SourceOption>(
      rs => ({
        value: { type: 'remoteSchema', remoteSchema: rs.name },
        label: rs.name,
      })
    );

    return [...tables, ...remoteSchemas];
  });
};
