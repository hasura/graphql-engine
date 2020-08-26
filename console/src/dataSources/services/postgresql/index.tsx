import React from 'react';
import { Table, TableColumn, ComputedField } from '../../types';
import { QUERY_TYPES, Operations } from '../../common';
import { PGFunction } from './types';
import { DataSourcesAPI, ColumnsInfoResult } from '../..';

export const isTable = (table: Table) => {
  return (
    table.table_type === 'TABLE' ||
    table.table_type === 'PARTITIONED TABLE' ||
    table.table_type === 'FOREIGN TABLE'
  );
};

export const displayTableName = (table: Table) => {
  // TODO: it shouldn't be here
  const tableName = table.table_name;

  return isTable(table) ? <span>{tableName}</span> : <i>{tableName}</i>;
};

export const getTableSupportedQueries = (table: Table) => {
  let supportedQueryTypes: Operations[];

  if (isTable(table)) {
    supportedQueryTypes = QUERY_TYPES;
  } else {
    // is View
    supportedQueryTypes = [];

    // Add insert/update permission if it is insertable/updatable as returned by pg
    if (table.view_info) {
      if (
        table.view_info.is_insertable_into === 'YES' ||
        table.view_info.is_trigger_insertable_into === 'YES'
      ) {
        supportedQueryTypes.push('insert');
      }

      supportedQueryTypes.push('select'); // to maintain order

      if (table.view_info.is_updatable === 'YES') {
        supportedQueryTypes.push('update');
        supportedQueryTypes.push('delete');
      } else {
        if (table.view_info.is_trigger_updatable === 'YES') {
          supportedQueryTypes.push('update');
        }

        if (table.view_info.is_trigger_deletable === 'YES') {
          supportedQueryTypes.push('delete');
        }
      }
    } else {
      supportedQueryTypes.push('select');
    }
  }

  return supportedQueryTypes;
};

export const getColumnType = (column: TableColumn) => {
  let columnType = column.data_type;

  if (columnType === 'USER-DEFINED') {
    columnType = column.udt_name;
  }

  return columnType;
};

export const isColumnAutoIncrement = (column: TableColumn) => {
  const columnDefault = column.column_default;

  const autoIncrementDefaultRegex = /^nextval\('(.*)_seq'::regclass\)$/;

  return !!(
    columnDefault &&
    columnDefault.match(new RegExp(autoIncrementDefaultRegex, 'gi'))
  );
};

export const arrayToPostgresArray = (arr: unknown[]) => {
  return `{${arr.join(',')}}`;
};

export const getFunctionSchema = (pgFunction: PGFunction) => {
  return pgFunction.function_schema;
};

export const getFunctionName = (pgFunction: PGFunction) => {
  return pgFunction.function_name;
};

export const getFunctionDefinition = (pgFunction: PGFunction) => {
  return pgFunction.function_definition;
};

export const getSchemaFunctions = (
  allFunctions: PGFunction[],
  fnSchema: string
) => {
  return allFunctions.filter(fn => getFunctionSchema(fn) === fnSchema);
};

export const findFunction = (
  allFunctions: PGFunction[],
  functionName: string,
  functionSchema: string
) => {
  return allFunctions.find(
    f =>
      getFunctionName(f) === functionName &&
      getFunctionSchema(f) === functionSchema
  );
};

export const getGroupedTableComputedFields = (
  table: Table,
  allFunctions: PGFunction[]
) => {
  const groupedComputedFields: {
    scalar: ComputedField[];
    table: ComputedField[];
  } = { scalar: [], table: [] };

  table.computed_fields.forEach(computedField => {
    const computedFieldFnDef = computedField.definition.function;
    const computedFieldFn = findFunction(
      allFunctions,
      computedFieldFnDef.name,
      computedFieldFnDef.schema
    );

    if (computedFieldFn && computedFieldFn.return_type_type === 'b') {
      groupedComputedFields.scalar.push(computedField);
    } else {
      groupedComputedFields.table.push(computedField);
    }
  });

  return groupedComputedFields;
};

const initQueries = {
  schemaList: {
    type: 'select',
    args: {
      table: {
        name: 'schemata',
        schema: 'information_schema',
      },
      columns: ['schema_name'],
      order_by: [{ column: 'schema_name', type: 'asc', nulls: 'last' }],
      where: {
        schema_name: {
          $nin: [
            'information_schema',
            'pg_catalog',
            'hdb_catalog',
            'hdb_views',
          ],
        },
      },
    },
  },
  loadTrackedFunctions: {
    type: 'select',
    args: {
      table: {
        name: 'hdb_function',
        schema: 'hdb_catalog',
      },
      columns: ['function_name', 'function_schema', 'is_system_defined'],
      order_by: [{ column: 'function_name', type: 'asc', nulls: 'last' }],
      where: {
        function_schema: '', // needs to be set later
      },
    },
  },
  loadTrackableFunctions: {
    type: 'select',
    args: {
      table: {
        name: 'hdb_function_agg',
        schema: 'hdb_catalog',
      },
      columns: [
        'function_name',
        'function_schema',
        'has_variadic',
        'function_type',
        'function_definition',
        'return_type_schema',
        'return_type_name',
        'return_type_type',
        'returns_set',
        {
          name: 'return_table_info',
          columns: ['table_schema', 'table_name'],
        },
      ],
      order_by: [{ column: 'function_name', type: 'asc', nulls: 'last' }],
      where: {
        function_schema: '', // needs to be set later
        has_variadic: false,
        returns_set: true,
        return_type_type: 'c', // COMPOSITE type
        return_table_info: {},
        $or: [
          {
            function_type: {
              $ilike: '%stable%',
            },
          },
          {
            function_type: {
              $ilike: '%immutable%',
            },
          },
        ],
      },
    },
  },
  loadNonTrackableFunctions: {
    type: 'select',
    args: {
      table: {
        name: 'hdb_function_agg',
        schema: 'hdb_catalog',
      },
      columns: [
        'function_name',
        'function_schema',
        'has_variadic',
        'function_type',
        'function_definition',
        'return_type_schema',
        'return_type_name',
        'return_type_type',
        'returns_set',
        {
          name: 'return_table_info',
          columns: ['table_schema', 'table_name'],
        },
      ],
      order_by: [{ column: 'function_name', type: 'asc', nulls: 'last' }],
      where: {
        function_schema: '', // needs to be set later
        $not: {
          has_variadic: false,
          returns_set: true,
          return_type_type: 'c', // COMPOSITE type
          return_table_info: {},
          $or: [
            {
              function_type: {
                $ilike: '%stable%',
              },
            },
            {
              function_type: {
                $ilike: '%immutable%',
              },
            },
          ],
        },
      },
    },
  },
};

const additionalColumnsInfoQuery = (schemaName: string) => ({
  type: 'select',
  args: {
    table: {
      name: 'columns',
      schema: 'information_schema',
    },
    columns: [
      'column_name',
      'table_name',
      'is_generated',
      'is_identity',
      'identity_generation',
    ],
    where: {
      table_schema: {
        $eq: schemaName,
      },
    },
  },
});

type ColumnsInfoPayload = {
  column_name: string;
  table_name: string;
  is_generated: string;
  is_identity: string;
  identity_generation: 'ALWAYS' | 'BY DEFAULT' | null;
};

const parseColumnsInfoResult = (data: ColumnsInfoPayload[]) => {
  let columnsInfo: ColumnsInfoResult = {};
  data
    .filter(
      (info: ColumnsInfoPayload) =>
        info.is_generated !== 'NEVER' || info.is_identity !== 'NO'
    )
    .forEach(
      ({
        column_name,
        table_name,
        is_generated,
        is_identity,
        identity_generation,
      }) => {
        columnsInfo = {
          ...columnsInfo,
          [table_name]: {
            ...columnsInfo[table_name],
            [column_name]: {
              is_generated: is_generated !== 'NEVER',
              is_identity: is_identity !== 'NO',
              identity_generation,
            },
          },
        };
      }
    );
  return columnsInfo;
};

export const postgres: DataSourcesAPI = {
  isTable,
  displayTableName,
  getFunctionSchema,
  getFunctionName,
  getFunctionDefinition,
  getSchemaFunctions,
  findFunction,
  getGroupedTableComputedFields,
  isColumnAutoIncrement,
  getTableSupportedQueries,
  getColumnType,
  arrayToPostgresArray,
  initQueries,
  additionalColumnsInfoQuery,
  parseColumnsInfoResult,
};
