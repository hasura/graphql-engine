import { NormalizedTable } from '@/dataSources/types';
import { useAppSelector } from '@/store';
import { getTableSchemaName } from './useTableSchema.utils';
import type { BrowseRowsTable } from './useTableSchema.utils';

export const useTableSchema = (table: BrowseRowsTable) => {
  const tableSchema: NormalizedTable | null = useAppSelector(state =>
    state.tables.allSchemas.find((schema: NormalizedTable) => {
      const tableSchemaName = getTableSchemaName(table);
      if (!tableSchemaName) {
        return null;
      }

      return (
        schema.table_schema === tableSchemaName &&
        schema.table_name === table?.name
      );
    })
  );

  return tableSchema;
};
