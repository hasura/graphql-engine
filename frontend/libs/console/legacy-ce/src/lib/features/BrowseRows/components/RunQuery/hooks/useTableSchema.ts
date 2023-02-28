import { NormalizedTable } from '../../../../../dataSources/types';
import { Table } from '../../../../hasura-metadata-types';
import { useAppSelector } from '../../../../../storeHooks';
import { getTableSchemaName } from './useTableSchema.utils';

export const useTableSchema = (table: Table) => {
  const tableSchema: NormalizedTable | null = useAppSelector(state =>
    state.tables.allSchemas.find((schema: NormalizedTable) => {
      const tableSchemaName = getTableSchemaName(table);
      if (!tableSchemaName) {
        return null;
      }

      return (
        schema.table_schema === tableSchemaName &&
        // This is a safe assumption since it's only going to be used for native database
        schema.table_name === (table as { name: string })?.name
      );
    })
  );

  return tableSchema;
};
