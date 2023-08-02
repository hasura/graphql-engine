import isObject from 'lodash/isObject';
import { QualifiedTable, TableConfig } from '../../../metadata/types';
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
import {
  isConsoleError,
  isEmpty,
} from '../../../components/Common/utils/jsUtils';
import { CockroachTable } from './types';
import {
  getColQuery,
  getFullQueryNameBase,
  getGraphQLQueryBase,
  getQueryWithNamespace,
} from '../../common';
import { WhereClause } from '../../../components/Common/utils/v1QueryUtils';
import { replaceAllStringOccurrences } from '../../common/index';
import { SourceCustomization } from '../../../features/hasura-metadata-types';

type Tables = ReduxState['tables'];

interface QueryBody {
  clauses: string;
  relationshipInfo: Relationship[];
}

// NOTE: official documentation https://www.cockroachlabs.com/docs/stable/data-types.html
export const CockroachDataTypes = {
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
    CockroachDataTypes.character.includes(type) ||
    CockroachDataTypes.dateTime.includes(type)
  ) {
    if (Array.isArray(value)) {
      return JSON.stringify(value);
    }
    return `"${value}"`;
  }

  if (CockroachDataTypes.numeric.includes(type)) {
    if (Array.isArray(value)) {
      return JSON.stringify(value);
    }
    return value;
  }

  return value;
};

const getFullQueryName = getFullQueryNameBase('public');

export const getRowsCountRequestBody = ({
  tables,
  tableConfiguration,
  dataSourceCustomization,
}: {
  tables: Tables;
  tableConfiguration: TableConfig;
  dataSourceCustomization: SourceCustomization;
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
      dataSourceCustomization,
      operation: 'select_aggregate',
    });

    const namespace = dataSourceCustomization?.root_fields?.namespace ?? '';
    return getQueryWithNamespace({
      queryName: 'query TableCount',
      namespace: namespace,
      innerQuery: `
        ${queryName} ${clauses && `(${clauses})`} {
          aggregate {
            count
          }
        }
      `,
    });
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
  dataSourceCustomization: SourceCustomization;
}): number => {
  const key = getFullQueryName({
    tableName: c.originalTable,
    schema: c.currentSchema,
    tableConfiguration: c.tableConfiguration,
    dataSourceCustomization: c.dataSourceCustomization,
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
  dataSourceCustomization,
}: {
  tables: Tables;
  isExport?: boolean;
  tableConfiguration: TableConfig;
  dataSourceCustomization: SourceCustomization;
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
    dataSourceCustomization,
    operation: 'select',
  });
  const aggregateName = getFullQueryName({
    tableName: originalTable,
    schema: currentSchema,
    tableConfiguration,
    dataSourceCustomization,
    operation: 'select_aggregate',
  });

  const namespace = dataSourceCustomization?.root_fields?.namespace ?? '';
  const queryBody = ({ clauses, relationshipInfo }: QueryBody) => {
    return getQueryWithNamespace({
      queryName: 'query TableRows',
      namespace: namespace,
      innerQuery: `
      ${queryName} ${clauses && `(${clauses})`}
      {
          ${getColQuery(
            view.query.columns,
            view.curFilter.limit,
            relationshipInfo,
            tableConfiguration,
            dataSourceCustomization
          ).join('\n')}
      }
      ${aggregateName}
      {
        aggregate {
          count
        }
      }
      `,
    });
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
    dataSourceCustomization: SourceCustomization;
  }
) => {
  try {
    const {
      originalTable,
      currentSchema,
      tableConfiguration,
      dataSourceCustomization,
    } = config!;

    const reversedCustomColumns = Object.entries(
      tableConfiguration?.column_config ?? {}
    ).reduce((acc: Record<string, string>, [col, colConfig]) => {
      if (colConfig.custom_name) acc[colConfig.custom_name] = col;
      return acc;
    }, {});

    const queryName = getFullQueryName({
      tableName: originalTable,
      schema: currentSchema,
      tableConfiguration,
      dataSourceCustomization,
      operation: 'select',
    });

    const namespace = dataSourceCustomization?.root_fields?.namespace ?? '';
    const hasNamespace = !!namespace;

    const results = hasNamespace
      ? data?.data[namespace][queryName]
      : data?.data[queryName];

    const rows = isEmpty(reversedCustomColumns)
      ? results
      : results?.map((row: Record<string, any>) =>
          Object.entries(row).reduce(
            (acc: Record<string, any>, [maybeCustomCol, col]) => {
              acc[reversedCustomColumns[maybeCustomCol] ?? maybeCustomCol] =
                col;
              return acc;
            },
            {}
          )
        );
    const aggregateName = getFullQueryName({
      tableName: originalTable,
      schema: currentSchema,
      tableConfiguration,
      dataSourceCustomization,
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
  const { tableConfiguration, dataSourceCustomization } = data;
  const columnConfig = tableConfiguration?.column_config ?? {};

  const processedData: Record<string, any> = {};
  Object.entries(data.insertObject).forEach(([key, value]) => {
    processedData[columnConfig[key]?.custom_name || key] = value;
  });
  const values = Object.entries(processedData).map(([key, value]) => {
    if (isObject(value)) {
      return `${key}: ${replaceAllStringOccurrences(
        JSON.stringify(value),
        '"',
        ''
      )}`;
    }
    return `${key}: ${typeof value === 'string' ? `"${value}"` : value}`;
  });
  const returning = Object.keys(processedData).join('\n');

  const queryName = getFullQueryName({
    tableName,
    schema,
    tableConfiguration,
    dataSourceCustomization,
    operation: 'insert',
  });

  const query = getQueryWithNamespace({
    queryName: 'mutation InsertRow',
    namespace: dataSourceCustomization?.root_fields?.namespace ?? '',
    innerQuery: `
      ${queryName}(objects: { ${values} }){
        returning {
          ${returning}
        }
      }
    `,
  });

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
  dataSourceCustomization: SourceCustomization,
  config: processInsertDataParameter[3]
) => {
  const { currentTable, currentSchema } = config!;
  const index = getFullQueryName({
    tableName: currentTable,
    schema: currentSchema,
    tableConfiguration,
    dataSourceCustomization,
    operation: 'insert',
  });

  const namespace = dataSourceCustomization?.root_fields?.namespace ?? '';
  const hasNamespace = !!namespace;

  const _result = result as {
    data: Record<string, Record<string, any>>;
  };

  const data = hasNamespace
    ? _result?.data[namespace][index]
    : _result?.data[index];

  const returnedFields = data?.returning;
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
  lTable: CockroachTable,
  rTable: CockroachTable
) => {
  if (rel.isObjRel) {
    if (
      ['reference', 'local'].includes(lTable.cockroach_table_type) &&
      rTable.cockroach_table_type === 'distributed'
    )
      return false;

    if (
      lTable.cockroach_table_type === 'distributed' &&
      rTable.cockroach_table_type === 'local'
    )
      return false;

    return true;
  }

  /* Array relationship rules */
  if (
    ['reference', 'local'].includes(lTable.cockroach_table_type) &&
    rTable.cockroach_table_type === 'distributed'
  )
    return false;

  if (
    lTable.cockroach_table_type === 'distributed' &&
    ['reference', 'local'].includes(rTable.cockroach_table_type)
  )
    return false;

  return true;
};

const getEditRowRequestBody = (data: {
  source: string;
  tableDef: QualifiedTable;
  tableConfiguration: TableConfig;
  dataSourceCustomization: SourceCustomization;
  set: Record<string, any>;
  where: Record<string, any>;
}) => {
  const { tableConfiguration, dataSourceCustomization } = data;
  const columnConfig = tableConfiguration?.column_config || {};
  const whereClause = Object.entries(data.where)
    .map(
      ([key, value]) =>
        `${columnConfig[key]?.custom_name || key}: {_eq: ${
          typeof value === 'string' ? `"${value}"` : value
        }}`
    )
    .join(', ');

  const setClause = Object.entries(data.set)
    .map(
      ([key, value]) =>
        `${columnConfig[key]?.custom_name || key}: ${
          typeof value === 'string' ? `"${value}"` : value
        }`
    )
    .join(', ');
  const { name: tableName, schema } = data.tableDef;
  const operationName = getFullQueryName({
    tableName,
    schema,
    tableConfiguration,
    dataSourceCustomization,
    operation: 'update',
  });

  const namespace = dataSourceCustomization?.root_fields?.namespace ?? '';
  const query = getQueryWithNamespace({
    queryName: 'mutation EditRow',
    namespace,
    innerQuery: `
      ${operationName}(where: {${whereClause}}, _set: {${setClause}}) {
        affected_rows
      }
    `,
  });

  return {
    query,
    variables: null,
  };
};

const processEditData = ({
  data,
  tableDef,
  tableConfiguration,
  dataSourceCustomization,
}: {
  tableDef: QualifiedTable;
  data: any;
  tableConfiguration: TableConfig;
  dataSourceCustomization: SourceCustomization;
}): number => {
  const { name: tableName, schema } = tableDef;
  const operationName = getFullQueryName({
    tableName,
    schema,
    tableConfiguration,
    dataSourceCustomization,
    operation: 'update',
  });

  const namespace = dataSourceCustomization?.root_fields?.namespace ?? '';
  const hasNamespace = !!namespace;

  const results = hasNamespace
    ? data?.data[namespace][operationName]
    : data?.data[operationName];

  return results.affected_rows;
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
  dataSourceCustomization,
}: {
  pkClause: WhereClause;
  tableName: string;
  schemaName: string;
  columnInfo: BaseTableColumn[];
  tableConfiguration: TableConfig;
  dataSourceCustomization: SourceCustomization;
}) => {
  const columnConfig = tableConfiguration?.column_config || {};

  const args = Object.keys(pkClause)
    .map(key => {
      let value = (pkClause as Record<string, any>)[key];
      const column = columnInfo.find(c => c.column_name === key);
      const columnName = columnConfig[key]?.custom_name || key;
      value = getFormattedValue(column?.data_type_name || 'varchar', value);
      return `${columnName}: {_eq: ${value}}`;
    })
    .join(',');
  const identifier = getFullQueryName({
    tableName,
    schema: schemaName,
    tableConfiguration,
    dataSourceCustomization,
    operation: 'delete',
  });

  const namespace = dataSourceCustomization?.root_fields?.namespace ?? '';
  const query = getQueryWithNamespace({
    queryName: 'mutation DeleteRows',
    namespace,
    innerQuery: `
      delete_row: ${identifier}(where: {${args}}) {
        affected_rows
      }
    `,
  });

  return {
    query,
    variables: null,
  };
};

const processDeleteRowData = (
  data: Record<string, any>,
  config: {
    dataSourceCustomization: SourceCustomization;
  }
) => {
  try {
    if (data.errors) throw new Error(data.errors[0].message);

    const namespace =
      config.dataSourceCustomization?.root_fields?.namespace ?? '';
    const hasNamespace = !!namespace;

    const results = hasNamespace
      ? data?.data[namespace]?.delete_row
      : data?.data?.delete_row;

    if (results?.affected_rows) return results?.affected_rows;
    throw new Error('Invalid response');
  } catch (err) {
    if (isConsoleError(err)) {
      throw new Error(err.message);
    }
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
  dataSourceCustomization,
}: {
  pkClauses: WhereClause[];
  tableName: string;
  schemaName: string;
  columnInfo: BaseTableColumn[];
  tableConfiguration: TableConfig;
  dataSourceCustomization: SourceCustomization;
}) => {
  const columnConfig = tableConfiguration?.column_config || {};
  const identifier = getFullQueryName({
    tableName,
    schema: schemaName,
    tableConfiguration,
    dataSourceCustomization,
    operation: 'delete',
  });
  const topLevelFields = pkClauses.map((pkClause, i) => {
    const args = Object.keys(pkClause)
      .map(key => {
        let value = (pkClause as Record<string, any>)[key];
        const column = columnInfo.find(c => c.column_name === key);
        const columnName = columnConfig[key]?.custom_name || key;
        value = getFormattedValue(column?.data_type_name || 'varchar', value);
        return `${columnName}: {_eq: ${value}}`;
      })
      .join(',');

    return `delete_row_${i}: ${identifier}(where: {${args}}) { affected_rows }`;
  });

  const namespace = dataSourceCustomization?.root_fields?.namespace ?? '';
  const query = getQueryWithNamespace({
    queryName: 'mutation BulkDeleteRows',
    namespace,
    innerQuery: `
      ${topLevelFields.join('\n')}
    `,
  });

  return {
    query,
    variables: null,
  };
};

const processBulkDeleteRowData = (
  data: Record<string, any>,
  config: {
    dataSourceCustomization: SourceCustomization;
  }
) => {
  try {
    if (data.errors) throw new Error(data.errors[0].message);

    if (data.data) {
      const namespace =
        config.dataSourceCustomization?.root_fields?.namespace ?? '';
      const hasNamespace = !!namespace;

      const results = hasNamespace ? data?.data[namespace] : data?.data;

      const res = Object.keys(results)
        .filter(key => results[key])
        .map(key => results[key].affected_rows)
        .reduce((a, b) => a + b, 0);
      return res;
    }
  } catch (err) {
    if (isConsoleError(err)) {
      throw err;
    }
  }
};

export const generateBulkDeleteRowRequest =
  (): GenerateBulkDeleteRowRequest => ({
    endpoint: Endpoints.graphQLUrl,
    getBulkDeleteRowRequestBody,
    processBulkDeleteRowData,
  });
