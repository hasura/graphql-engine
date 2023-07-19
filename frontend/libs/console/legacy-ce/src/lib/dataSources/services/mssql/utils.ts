import {
  getFullQueryNameBase,
  getGraphQLQueryBase,
  getQueryWithNamespace,
  QueryBody,
} from './../../common';
import { TableConfig } from './../../../metadata/types';
import Endpoints from '../../../Endpoints';
import { ReduxState } from '../../../types';
import { Relationship } from '../../types';
import { isEmpty } from '../../../components/Common/utils/jsUtils';
import { SourceCustomization } from '../../../features/hasura-metadata-types';
import { Column, getQueryName } from '../../common/utils';

type Tables = ReduxState['tables'];

export const SQLServerTypes = {
  character: [
    'char',
    'varchar',
    'text',
    'nchar',
    'nvarchar',
    'binary',
    'vbinary',
    'image',
  ],
  numeric: [
    'bit',
    'tinyint',
    'smallint',
    'int',
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

const getFullQueryName = getFullQueryNameBase('dbo');

const getFormattedValue = (
  type: string,
  value: any
): string | number | undefined => {
  if (
    SQLServerTypes.character.includes(type) ||
    SQLServerTypes.dateTime.includes(type)
  ) {
    if (Array.isArray(value)) {
      return JSON.stringify(value);
    }
    return `"${value}"`;
  }

  if (SQLServerTypes.numeric.includes(type)) {
    if (Array.isArray(value)) {
      return JSON.stringify(value);
    }
    return value;
  }
  return undefined;
};

const getColQuery = (
  cols: (string | Column)[],
  limit: number,
  relationships: Relationship[],
  tableConfiguration: TableConfig,
  dataSourceCustomization: SourceCustomization
): string[] => {
  return cols.map(column => {
    const columnConfig = tableConfiguration?.column_config ?? {};

    if (typeof column === 'string') {
      return columnConfig[column]?.custom_name ?? column;
    }

    const queryName = getQueryName({
      column,
      tableConfiguration,
      dataSourceCustomization,
    });

    const rel = relationships.find((r: any) => r.rel_name === column.name);
    return `${queryName} ${
      rel?.rel_type === 'array' ? `(limit: ${limit})` : ''
    } {
      ${getColQuery(
        column.columns,
        limit,
        relationships,
        tableConfiguration,
        dataSourceCustomization
      ).join('\n')} }`;
  });
};

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
  const tableName = tableConfiguration?.custom_name ?? originalTable;
  const queryName = getFullQueryName({
    tableName,
    schema: currentSchema,
    tableConfiguration,
    dataSourceCustomization,
    operation: 'select',
  });
  const aggregateName = getFullQueryName({
    tableName,
    schema: currentSchema,
    tableConfiguration,
    dataSourceCustomization,
    operation: 'select_aggregate',
  });

  const namespace = dataSourceCustomization?.root_fields?.namespace ?? '';
  const queryBody = ({ clauses, relationshipInfo }: QueryBody) => {
    return getQueryWithNamespace({
      queryName: 'query TableRows',
      namespace,
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

const processTableRowData = (
  data: any,
  config?: {
    originalTable: string;
    currentSchema: string;
    tableConfiguration: TableConfig;
    dataSourceCustomization: SourceCustomization;
  }
) => {
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

  const tableName = tableConfiguration?.custom_name || originalTable;
  const queryName = getFullQueryName({
    tableName,
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
            acc[reversedCustomColumns[maybeCustomCol] ?? maybeCustomCol] = col;
            return acc;
          },
          {}
        )
      );
  const estimatedCount =
    data?.data[`${queryName}_aggregate`]?.aggregate?.count ?? rows.length;
  return { estimatedCount, rows };
};

export const generateTableRowRequest = () => ({
  endpoint: Endpoints.graphQLUrl,
  getTableRowRequestBody,
  processTableRowData,
});

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
    operation: 'select_aggregate',
    dataSourceCustomization: c.dataSourceCustomization,
  });
  return c.data?.data?.[key]?.aggregate?.count;
};

export const generateRowsCountRequest = () => ({
  getRowsCountRequestBody,
  endpoint: Endpoints.graphQLUrl,
  processCount,
});

export const sqlHandleApostrophe = (rawText: string) => {
  return rawText ? rawText.replace(/'/g, "''") : rawText;
};
