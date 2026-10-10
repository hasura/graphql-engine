import { TrackableTable } from '@hasura/metadata/api';
import { Metadata, MetadataTable, Table } from '@hasura/shared/types';
import { extractTableInfo } from '@hasura/shared/utils';
import { areTablesEqual, MetadataSelectors } from '@hasura/metadata/helpers';
import { IntrospectedTable, TableColumn } from '../../driver';

type Payload = {
  trackedTables: TrackableTable[];
  untrackedTables: TrackableTable[];
};

export const splitByTracked = ({
  metadataTables,
  introspectedTables,
  schemaFilter,
}: {
  metadataTables: MetadataTable[];
  introspectedTables: IntrospectedTable[];
  schemaFilter?: string;
}): Payload => {
  return introspectedTables.reduce<Payload>(
    (payload, table) => {
      const qualifiedTable = extractTableInfo(table.table);

      if (schemaFilter && qualifiedTable?.name !== schemaFilter) {
        // skip if we have a schema filter active and the qualified table array does not include the schema name
        return payload;
      }

      const trackedTable = metadataTables.find((t) =>
        areTablesEqual(t.table, table.table),
      );

      const key: keyof Payload = trackedTable
        ? 'trackedTables'
        : 'untrackedTables';

      payload[key] = [
        ...payload[key],
        {
          ...table,
          id: table.name,
          is_tracked: !!trackedTable,
        },
      ];

      return payload;
    },
    { trackedTables: [], untrackedTables: [] },
  );
};
export const adaptUntrackedTables =
  (trackedTables: Table[]) => (introspectedTables: IntrospectedTable[]) => {
    return introspectedTables
      .filter((introspectedTable) => {
        const isTableTracked = trackedTables.find((t) =>
          areTablesEqual(t, introspectedTable.table),
        );

        return !isTableTracked;
      })
      .map((untrackedTable) => ({
        ...untrackedTable,
        id: untrackedTable.name,
        is_tracked: false,
      }));
  };

export const adaptTrackedTables =
  (trackedTables: Table[]) => (introspectedTables: IntrospectedTable[]) => {
    return introspectedTables
      .filter((introspectedTable) => {
        const isTableTracked = trackedTables.find((t) =>
          areTablesEqual(t, introspectedTable.table),
        );

        return isTableTracked;
      })
      .map((trackedTable) => ({
        ...trackedTable,
        id: trackedTable.name,
        is_tracked: true,
      }));
  };

export const selectTrackedTables =
  (m: Metadata | undefined) => (dataSourceName: string) => {
    if (!m) {
      return [];
    }

    return MetadataSelectors.getTables(dataSourceName)(m)?.map((t) => t.table);
  };

export const selectPrimaryKeys = (columns: TableColumn[]) => {
  return columns.filter((col) => col.isPrimaryKey);
};
