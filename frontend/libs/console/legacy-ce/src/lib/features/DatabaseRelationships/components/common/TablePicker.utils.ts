import { areTablesEqual } from '../../../hasura-metadata-api';
import { Source, Table } from '../../../hasura-metadata-types';
import { SourceSelectorItem } from './SourcePicker/SourcePicker.types';
import { TablePickerProps } from './TablePicker.types';

export const filterMetadataSources = (
  sources: Source[],
  filterDataSource: TablePickerProps['filterDataSource']
) =>
  sources.filter(source => {
    if (!filterDataSource) {
      return source;
    }
    return source.name === filterDataSource;
  });

export const mapMetadataSourceToSelectorItems = (
  sources: Source[],
  filterDataSource: TablePickerProps['filterDataSource']
) => {
  const items: SourceSelectorItem[] = [];
  filterMetadataSources(sources, filterDataSource).forEach(source => {
    source.tables.forEach(table => {
      const sourcePickerItem: SourceSelectorItem = {
        type: 'table',
        value: { table: table.table, dataSourceName: source.name },
      };
      items.push(sourcePickerItem);
    });
  });

  return items;
};

type GetDefaultSourceSelectorItemArgs = {
  sourceSelectorItems: SourceSelectorItem[];
  dataSourceName: string | undefined;
  table: Table;
};

export const getDefaultSourceSelectorItem = ({
  sourceSelectorItems,
  dataSourceName,
  table,
}: GetDefaultSourceSelectorItemArgs) =>
  sourceSelectorItems.find(
    item =>
      (item.type === 'table' &&
        item.value.dataSourceName === dataSourceName &&
        areTablesEqual(item.value.table, table)) ||
      (item.type === 'remoteSchema' &&
        item.value.remoteSchemaName === dataSourceName &&
        areTablesEqual(item.value, table))
  );
