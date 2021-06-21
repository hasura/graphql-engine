import { QualifiedTable, TableConfig } from './../../../metadata/types';
import Endpoints from '../../../Endpoints';
import { ReduxState } from '../../../types';
import {
  Relationship,
  generateInsertRequestType,
  RelType,
  GenerateDeleteRowRequest,
  GenerateBulkDeleteRowRequest,
  BaseTableColumn,
} from '../../types';
import { isEmpty } from '../../../components/Common/utils/jsUtils';
import { CitusTable } from './types';
import {
  getColQuery,
  getFullQueryNameBase,
  getGraphQLQueryBase,
} from '../../common';
import { WhereClause } from '../../../components/Common/utils/v1QueryUtils';

type Tables = ReduxState['tables'];

interface QueryBody {
  clauses: string;
  relationshipInfo: Relationship[];
}

export const CitusDataTypes = {
  character: [
    'char',
    'varchar',
    'text',
    'nchar',
    'nvarchar',
    'binary',
    'vbinary',
    'image',
    'character',
    'citext',
  ],
  numeric: [
    'smallint',
    'integer',
    'double precision',
    'int',
    'int2',
    'int4',
    'int8',
    'bigint',
    'decimal',
    'numeric',
    'smallmoney',
    'money',
    'float',
    'real',
  ],
  dateTime: [
    'datetime',
    'smalldatetime',
    'date',
    'time',
    'datetimeoffset',
    'timestamp',
  ],
  user_defined: [],
};

export const operators = [
  { name: 'equals', value: '$eq', graphqlOp: '_eq' },
  { name: 'not equals', value: '$ne', graphqlOp: '_neq' },
  { name: '>', value: '$gt', graphqlOp: '_gt' },
  { name: '<', value: '$lt', graphqlOp: '_lt' },
  { name: '>=', value: '$gte', graphqlOp: '_gte' },
  { name: '<=', value: '$lte', graphqlOp: '_lte' },
];

const getFormattedValue = (
  type: string,
  value: any
): string | number | undefined => {
  if (
    CitusDataTypes.character.includes(type) ||
    CitusDataTypes.dateTime.includes(type)
  )
    return `"${value}"`;

  if (CitusDataTypes.numeric.includes(type)) return value;
};

const getFullQueryName = getFullQueryNameBase('public');

export const getRowsCountRequestBody = ({
  tables,
  tableConfiguration,
}: {
  tables: Tables;
  tableConfiguration: TableConfig;
}) => {
  const {
    currentTable: originalTable,
    currentSchema,
    allSchemas,
    view,
  } = tables;
  const queryBody = ({ clauses }: QueryBody) => {
    const queryName = getFullQueryName({
      tableName: originalTable,
      schema: currentSchema,
      tableConfiguration,
      operation: 'select_aggregate',
    });
    return `query TableCount {
      ${queryName} ${clauses && `(${clauses})`} {
        aggregate {
          count
        }
      }
    }`;
  };

  return {
    query: getGraphQLQueryBase({
      allSchemas,
      view,
      originalTable,
      currentSchema,
      isExport: true,
      tableConfiguration,
      queryBody,
      getFormattedValue,
    }),
    variables: null,
    operationName: 'TableCount',
  };
};

const processCount = (c: {
  data: any;
  currentSchema: string;
  originalTable: string;
  tableConfiguration: TableConfig;
}): number => {
  const key = getFullQueryName({
    tableName: c.originalTable,
    schema: c.currentSchema,
    tableConfiguration: c.tableConfiguration,
    operation: 'select_aggregate',
  });
  return c.data?.data[key]?.aggregate?.count;
};

export const generateRowsCountRequest = () => ({
  getRowsCountRequestBody,
  endpoint: Endpoints.graphQLUrl,
  processCount,
});

export const getTableRowRequestBody = ({
  tables,
  isExport,
  tableConfiguration,
}: {
  tables: Tables;
  isExport?: boolean;
  tableConfiguration: TableConfig;
}) => {
  const {
    currentTable: originalTable,
    view,
    allSchemas,
    currentSchema,
  } = tables;
  const queryName = getFullQueryName({
    tableName: originalTable,
    schema: currentSchema,
    tableConfiguration,
    operation: 'select',
  });
  const aggregateName = getFullQueryName({
    tableName: originalTable,
    schema: currentSchema,
    tableConfiguration,
    operation: 'select_aggregate',
  });
  const queryBody = ({ clauses, relationshipInfo }: QueryBody) => {
    return `query TableRows {
      ${queryName} ${clauses && `(${clauses})`} {
          ${getColQuery(
            view.query.columns,
            view.curFilter.limit,
            relationshipInfo,
            tableConfiguration
          ).join('\n')} 
    }
    ${aggregateName} {
      aggregate {
        count
      }
    }
  }`;
  };

  return {
    query: getGraphQLQueryBase({
      allSchemas,
      view,
      originalTable,
      currentSchema,
      isExport,
      tableConfiguration,
      queryBody,
      getFormattedValue,
    }),
    variables: null,
    operationName: 'TableRows',
  };
};

export const processTableRowData = (
  data: any,
  config: {
    originalTable: string;
    currentSchema: string;
    tableConfiguration: TableConfig;
  }
) => {
  try {
    const { originalTable, currentSchema, tableConfiguration } = config!;

    const reversedCustomColumns = Object.entries(
      tableConfiguration?.custom_column_names ?? {}
    ).reduce((acc: Record<string, string>, [col, customCol]) => {
      acc[customCol] = col;
      return acc;
    }, {});

    const queryName = getFullQueryName({
      tableName: originalTable,
      schema: currentSchema,
      tableConfiguration,
      operation: 'select',
    });
    const results = data?.data[queryName];

    const rows = isEmpty(reversedCustomColumns)
      ? results
      : results?.map((row: Record<string, any>) =>
          Object.entries(row).reduce(
            (acc: Record<string, any>, [maybeCustomCol, col]) => {
              acc[
                reversedCustomColumns[maybeCustomCol] ?? maybeCustomCol
              ] = col;
              return acc;
            },
            {}
          )
        );
    const aggregateName = getFullQueryName({
      tableName: originalTable,
      schema: currentSchema,
      tableConfiguration,
      operation: 'select_aggregate',
    });
    const estimatedCount =
      data?.data[aggregateName]?.aggregate?.count ?? rows.length;
    return { estimatedCount, rows };
  } catch (err) {
    throw new Error('data source is inconsistent');
  }
};

export const generateTableRowRequest = () => ({
  endpoint: Endpoints.graphQLUrl,
  getTableRowRequestBody,
  processTableRowData,
});

const getInsertRequestBody = (
  data: Parameters<generateInsertRequestType['getInsertRequestBody']>[0]
): ReturnType<generateInsertRequestType['getInsertRequestBody']> => {
  const { name: tableName, schema } = data.tableDef;
  const { tableConfiguration } = data;
  const customColumnNames = tableConfiguration?.custom_column_names ?? {};

  const processedData: Record<string, any> = {};
  Object.entries(data.insertObject).forEach(([key, value]) => {
    processedData[customColumnNames[key] || key] = value;
  });
  const values = Object.entries(processedData).map(([key, value]) => {
    return `${key}: ${typeof value === 'string' ? `"${value}"` : value}`;
  });
  const returning = Object.keys(processedData).join('\n');

  const queryName = getFullQueryName({
    tableName,
    schema,
    tableConfiguration,
    operation: 'insert',
  });

  const query = `
  mutation InsertRow {
     ${queryName}(objects: { ${values} }){
         returning { 
           ${returning}
         }
     }
  }
  `;

  return {
    query,
    variables: null,
  };
};

type processInsertDataParameter = Parameters<
  generateInsertRequestType['processInsertData']
>;

const processInsertData = (
  result: processInsertDataParameter[0],
  tableConfiguration: TableConfig,
  config: processInsertDataParameter[2]
) => {
  const { currentTable, currentSchema } = config!;
  const index = getFullQueryName({
    tableName: currentTable,
    schema: currentSchema,
    tableConfiguration,
    operation: 'insert',
  });
  const returnedFields = (result as {
    data: Record<string, Record<string, any>>;
  })?.data?.[index]?.returning;
  return {
    affectedRows: 1,
    returnedFields:
      returnedFields?.length === 1 ? returnedFields[0] : returnedFields,
  };
};

export const generateInsertRequest = () => ({
  endpoint: Endpoints.graphQLUrl,
  processInsertData,
  getInsertRequestBody,
});

export const isRelationshipValid = (
  rel: RelType,
  lTable: CitusTable,
  rTable: CitusTable
) => {
  if (rel.isObjRel) {
    if (
      ['reference', 'local'].includes(lTable.citus_table_type) &&
      rTable.citus_table_type === 'distributed'
    )
      return false;

    if (
      lTable.citus_table_type === 'distributed' &&
      rTable.citus_table_type === 'local'
    )
      return false;

    return true;
  }

  /* Array relationship rules */
  if (
    ['reference', 'local'].includes(lTable.citus_table_type) &&
    rTable.citus_table_type === 'distributed'
  )
    return false;

  if (
    lTable.citus_table_type === 'distributed' &&
    ['reference', 'local'].includes(rTable.citus_table_type)
  )
    return false;

  return true;
};

const getEditRowRequestBody = (data: {
  source: string;
  tableDef: QualifiedTable;
  tableConfiguration: TableConfig;
  set: Record<string, any>;
  where: Record<string, any>;
}) => {
  const { tableConfiguration } = data;
  const customColumnNames = tableConfiguration?.custom_column_names || {};
  const whereClause = Object.entries(data.where)
    .map(
      ([key, value]) =>
        `${customColumnNames[key] || key}: {_eq: ${
          typeof value === 'string' ? `"${value}"` : value
        }}`
    )
    .join(', ');

  const setClause = Object.entries(data.set)
    .map(
      ([key, value]) =>
        `${customColumnNames[key] || key}: ${
          typeof value === 'string' ? `"${value}"` : value
        }`
    )
    .join(', ');
  const { name: tableName, schema } = data.tableDef;
  const operationName = getFullQueryName({
    tableName,
    schema,
    tableConfiguration,
    operation: 'update',
  });

  const query = `mutation {
    ${operationName}(where: {${whereClause}}, _set: {${setClause}}) {
      affected_rows
    }
  }`;
  return {
    query,
    variables: null,
  };
};

const processEditData = ({
  data,
  tableDef,
  tableConfiguration,
}: {
  tableDef: QualifiedTable;
  data: any;
  tableConfiguration: TableConfig;
}): number => {
  const { name: tableName, schema } = tableDef;
  const operationName = getFullQueryName({
    tableName,
    schema,
    tableConfiguration,
    operation: 'update',
  });
  return data?.data[operationName].affected_rows;
};

export const generateEditRowRequest = () => ({
  endpoint: Endpoints.graphQLUrl,
  processEditData,
  getEditRowRequestBody,
});

const getDeleteRowRequestBody = ({
  pkClause,
  tableName,
  schemaName,
  columnInfo,
  tableConfiguration,
}: {
  pkClause: WhereClause;
  tableName: string;
  schemaName: string;
  columnInfo: BaseTableColumn[];
  tableConfiguration: TableConfig;
}) => {
  const customColumns = tableConfiguration?.custom_column_names;

  const args = Object.keys(pkClause)
    .map(key => {
      let value = (pkClause as Record<string, any>)[key];
      const column = columnInfo.find(c => c.column_name === key);
      const columnName =
        customColumns && customColumns[key] ? customColumns[key] : key;
      value = getFormattedValue(column?.data_type_name || 'varchar', value);
      return `${columnName}: {_eq: ${value}}`;
    })
    .join(',');
  const identifier = getFullQueryName({
    tableName,
    schema: schemaName,
    tableConfiguration,
    operation: 'delete',
  });
  const query = `mutation DeleteRows {
    delete_row: ${identifier}(where: {${args}}) {
      affected_rows
    }
  }`;
  return {
    query,
    variables: null,
  };
};

const processDeleteRowData = (data: Record<string, any>) => {
  try {
    if (data.errors) throw new Error(data.errors[0].message);
    if (data?.data?.delete_row?.affected_rows)
      return data?.data?.delete_row?.affected_rows;
    throw new Error('Invalid response');
  } catch (err) {
    throw new Error(err.message);
  }
};

export const generateDeleteRowRequest = (): GenerateDeleteRowRequest => ({
  endpoint: Endpoints.graphQLUrl,
  getDeleteRowRequestBody,
  processDeleteRowData,
});

const getBulkDeleteRowRequestBody = ({
  pkClauses,
  tableName,
  schemaName,
  columnInfo,
  tableConfiguration,
}: {
  pkClauses: WhereClause[];
  tableName: string;
  schemaName: string;
  columnInfo: BaseTableColumn[];
  tableConfiguration: TableConfig;
}) => {
  const customColumns = tableConfiguration?.custom_column_names;
  const identifier = getFullQueryName({
    tableName,
    schema: schemaName,
    tableConfiguration,
    operation: 'delete',
  });
  const topLevelFields = pkClauses.map((pkClause, i) => {
    const args = Object.keys(pkClause)
      .map(key => {
        let value = (pkClause as Record<string, any>)[key];
        const column = columnInfo.find(c => c.column_name === key);
        const columnName =
          customColumns && customColumns[key] ? customColumns[key] : key;
        value = getFormattedValue(column?.data_type_name || 'varchar', value);
        return `${columnName}: {_eq: ${value}}`;
      })
      .join(',');

    return `delete_row_${i}: ${identifier}(where: {${args}}) { affected_rows }`;
  });
  const query = `mutation MyMutation {
    ${topLevelFields.join('\n')}
  }`;
  return {
    query,
    variables: null,
  };
};

const processBulkDeleteRowData = (data: Record<string, any>) => {
  try {
    if (data.errors) throw new Error(data.errors[0].message);

    if (data.data) {
      const res = Object.keys(data.data)
        .filter(key => data.data[key])
        .map(key => data.data[key].affected_rows)
        .reduce((a, b) => a + b, 0);
      return res;
    }
  } catch (err) {
    throw new Error(err.message);
  }
};

export const generateBulkDeleteRowRequest = (): GenerateBulkDeleteRowRequest => ({
  endpoint: Endpoints.graphQLUrl,
  getBulkDeleteRowRequestBody,
  processBulkDeleteRowData,
});
