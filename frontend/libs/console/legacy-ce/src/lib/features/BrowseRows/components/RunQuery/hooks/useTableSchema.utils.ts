import { Table } from '../../../../hasura-metadata-types';
import { BigQueryTable, PostgresTable } from '../../../../DataSource';

export const getTableSchemaName = (table: Table) => {
  const isObject =
    table && typeof table === 'object' && Object.keys(table).length > 0;

  if (isObject && 'schema' in table) {
    const sqlTable = table as PostgresTable;
    return sqlTable?.schema;
  }
  if (isObject && 'dataset' in table) {
    const bigQueryTable = table as BigQueryTable;
    return bigQueryTable?.dataset;
  }

  console.error(
    `Invalid Table object provided (missing schema or dataset), ${JSON.stringify(
      table
    )}`
  );

  return undefined;
};
