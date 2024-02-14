import { Feature, IntrospectedTable } from '../../DataSource';
import { MetadataSelectors, areTablesEqual } from '../../hasura-metadata-api';
import { Metadata, Table } from '../../hasura-metadata-types';
import { getQualifiedTable } from './utils';

export type PayloadTable = {
  id: string;
  is_tracked: boolean;
  name: string;
  table: unknown;
  type: string;
};
type Payload = {
  trackedTables: PayloadTable[];
  untrackedTables: PayloadTable[];
};

export const splitByTracked = ({
  metadataTables,
  introspectedTables,
  schemaFilter,
}: {
  metadataTables: Table[];
  introspectedTables: Feature | IntrospectedTable[];
  schemaFilter?: string;
}): Payload => {
  if (introspectedTables === Feature.NotImplemented)
    return { untrackedTables: [], trackedTables: [] };

  return introspectedTables.reduce<Payload>(
    (payload, table) => {
      const qualifiedTable = getQualifiedTable(table.table);

      if (schemaFilter && !qualifiedTable.includes(schemaFilter)) {
        // skip if we have a schema filter active and the qualified table array does not include the schema name
        return payload;
      }

      const isTableTracked = metadataTables.find(t =>
        areTablesEqual(t, table.table)
      );

      const key: keyof Payload = isTableTracked
        ? 'trackedTables'
        : 'untrackedTables';

      payload[key] = [
        ...payload[key],
        { ...table, id: table.name, is_tracked: !!isTableTracked },
      ];

      return payload;
    },
    { trackedTables: [], untrackedTables: [] }
  );
};
export const adaptUntrackedTables =
  (trackedTables: Table[]) =>
  (introspectedTables: Feature | IntrospectedTable[]) => {
    if (introspectedTables === Feature.NotImplemented) return [];

    return introspectedTables
      .filter(introspectedTable => {
        const isTableTracked = trackedTables.find(t =>
          areTablesEqual(t, introspectedTable.table)
        );

        return !isTableTracked;
      })
      .map(untrackedTable => ({
        ...untrackedTable,
        id: untrackedTable.name,
        is_tracked: false,
      }));
  };

export const adaptTrackedTables =
  (trackedTables: Table[]) =>
  (introspectedTables: Feature | IntrospectedTable[]) => {
    if (introspectedTables === Feature.NotImplemented) return [];

    return introspectedTables
      .filter(introspectedTable => {
        const isTableTracked = trackedTables.find(t =>
          areTablesEqual(t, introspectedTable.table)
        );

        return isTableTracked;
      })
      .map(trackedTable => ({
        ...trackedTable,
        id: trackedTable.name,
        is_tracked: true,
      }));
  };

export const selectTrackedTables =
  (m: Metadata) => (dataSourceName: string) => {
    return MetadataSelectors.getTables(dataSourceName)(m)?.map(t => t.table);
  };
