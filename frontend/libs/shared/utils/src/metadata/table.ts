import {
  DatasetTable,
  GDCTable,
  QualifiedTable,
  SchemaTable,
  Table,
} from '@hasura/shared/types';

export const isSchemaTable = (table: unknown): table is SchemaTable =>
  table !== null &&
  typeof table === 'object' &&
  'schema' in table &&
  'name' in table;

export const isDatasetTable = (table: unknown): table is DatasetTable =>
  table !== null &&
  typeof table === 'object' &&
  'dataset' in table &&
  'name' in table;

export const isGDCTable = (table: unknown): table is GDCTable =>
  Array.isArray(table) && table?.every((t) => typeof t === 'string');

export const isGDCFunction = isGDCTable;

export const extractTableInfo = (table: Table): QualifiedTable | null => {
  if (isSchemaTable(table)) {
    return table;
  }

  if (isDatasetTable(table)) {
    return {
      name: table.name,
      schema: table.dataset,
    };
  }

  if (isGDCTable(table)) {
    return table.length === 1
      ? { name: table[0], schema: '' }
      : { name: table[1], schema: table[0] };
  }

  if (typeof table === 'string') {
    return {
      name: table,
      schema: '',
    };
  }

  return null;
};

const NO_NAME = 'Empty Object';

/*
this function isn't entirely generic but it will hold for the current set of native DBs we have & GDC as well
*/
export const getTableDisplayName = (
  table: unknown,
  wrapCharacter = '',
  separator = '.',
): string => {
  let schema = '';
  let name = '';

  if (!table) {
    return NO_NAME;
  }

  if (Array.isArray(table)) {
    if (!table.length) {
      return NO_NAME;
    }

    if (table.length === 1) {
      name = table[0];
    } else {
      schema = table[0];
      name = table[1];
    }
  } else if (typeof table === 'string' || typeof table === 'number') {
    name = String(table);
  } else if (typeof table === 'object') {
    if (isSchemaTable(table)) {
      name = table.name;
      schema = table.schema;
    } else if (isDatasetTable(table)) {
      name = table.name;
      schema = table.dataset;
    } else if ('table_name' in table && 'table_schema' in table) {
      const t = table as { table_name: string; table_schema: string };
      name = t.table_name;
      schema = t.table_schema;
    } else if ('name' in table) {
      name = (table as { name: string }).name;
    } else {
      const values = Object.keys(table)
        .sort()
        .map((key) => (table as Record<string, unknown>)[key]);
      if (values.length && values.every((v) => typeof v === 'string')) {
        return (values as string[]).join(separator);
      }
      return JSON.stringify(table);
    }
  }

  if (!schema && !name) {
    return NO_NAME;
  }

  if (!schema) {
    return wrapCharacter ? `${wrapCharacter}${name}${wrapCharacter}` : name;
  }

  return `${wrapCharacter}${schema}${wrapCharacter}${separator}${wrapCharacter}${name}${wrapCharacter}`;
};

export const getTableLabel = ({
  dataSourceName,
  table,
}: {
  dataSourceName: string;
  table: Table;
}) => {
  if (isSchemaTable(table)) {
    return `${dataSourceName} / ${table.schema} / ${table.name}`;
  }

  if (isDatasetTable(table)) {
    return `${dataSourceName} / ${table.dataset} / ${table.name}`;
  }

  if (isGDCTable(table)) {
    return `${dataSourceName} / ${table.join(' /')}`;
  }

  return '';
};
