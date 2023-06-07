import { Feature, IntrospectedTable } from '../../DataSource';
import { MetadataSelectors, areTablesEqual } from '../../hasura-metadata-api';
import { Metadata, Table } from '../../hasura-metadata-types';

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
