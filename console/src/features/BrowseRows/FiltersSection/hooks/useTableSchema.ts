import { NormalizedTable } from '@/dataSources/types';
import { useAppSelector } from '@/store';

// NOTE: temporary type, to be replaced when GDC is ready
export type Table = { schema: string; name: string };

export const useTableSchema = (table: Table) => {
  const tableSchema = useAppSelector(state =>
    state.tables.allSchemas.find((schema: NormalizedTable) => {
      return (
        schema.table_schema === table?.schema &&
        schema.table_name === table?.name
      );
    })
  );

  return tableSchema;
};
