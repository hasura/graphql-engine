import { SourceCustomization } from '../../features/hasura-metadata-types';
import { TableConfig } from '../../metadata/types';

export type Column = { name: string; columns: string[] };

export type QueryNameArgs = {
  column: Column;
  tableConfiguration: TableConfig;
  dataSourceCustomization: SourceCustomization;
};

export const getQueryName = ({
  column,
  tableConfiguration,
  dataSourceCustomization,
}: QueryNameArgs) => {
  const columnConfig = tableConfiguration?.column_config ?? {};

  const prefix = dataSourceCustomization.root_fields?.prefix ?? '';
  const suffix = dataSourceCustomization.root_fields?.suffix ?? '';

  const columnName = column.name;

  if (columnConfig[columnName]?.custom_name) {
    return `${prefix}${columnConfig[columnName].custom_name}${suffix}`;
  }

  if (columnConfig[columnName]?.custom_name) {
    return `${prefix}${columnConfig[columnName].custom_name}${suffix}`;
  }

  if (tableConfiguration?.custom_root_fields?.select) {
    return `${prefix}${tableConfiguration.custom_root_fields.select}${suffix}`;
  }

  return `${prefix}${columnName}${suffix}`;
};
