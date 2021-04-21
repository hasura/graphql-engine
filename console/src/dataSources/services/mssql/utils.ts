import { TableEntry } from './../../../metadata/types';
import {
  OrderBy,
  WhereClause,
} from '../../../components/Common/utils/v1QueryUtils';
import Endpoints from '../../../Endpoints';
import { ReduxState } from '../../../types';
import { BaseTableColumn, Relationship, Table } from '../../types';
import { isEmpty } from '../../../components/Common/utils/jsUtils';

type Tables = ReduxState['tables'];
interface GetGraphQLQuery {
  allSchemas: Table[];
  view: Tables['view'];
  originalTable: string;
  currentSchema: string;
  isExport?: boolean;
  tableConfiguration: TableEntry['configuration'];
}

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

const getFormattedValue = (
  type: string,
  value: any
): string | number | undefined => {
  if (
    SQLServerTypes.character.includes(type) ||
    SQLServerTypes.dateTime.includes(type)
  )
    return `"${value}"`;

  if (SQLServerTypes.numeric.includes(type)) return value;
};

const RqlToGraphQlOp = (op: string) => {
  if (!op || !op?.startsWith('$')) return 'none';
  return (
    operators.find(_op => _op.value === op)?.graphqlOp ?? op.replace('$', '_')
  );
};

const generateWhereClauseQueryString = (
  wheres: WhereClause[],
  columnTypeInfo: BaseTableColumn[],
  tableConfiguration: TableEntry['configuration']
): string | null => {
  const customColumns = tableConfiguration?.custom_column_names ?? {};
  const whereClausesArr = wheres.map((i: Record<string, any>) => {
    const columnName = Object.keys(i)[0];
    const RqlOperator = Object.keys(i[columnName])[0];
    const value = i[columnName][RqlOperator];
    const type = columnTypeInfo?.find(c => c.column_name === columnName)
      ?.data_type_name;
    return `${customColumns[columnName] ?? columnName}: {${RqlToGraphQlOp(
      RqlOperator
    )}: ${getFormattedValue(type || 'varchar', value)} }`;
  });
  return whereClausesArr.length
    ? `where: {${whereClausesArr.join(',')}}`
    : null;
};

const generateSortClauseQueryString = (
  sorts: OrderBy[],
  tableConfiguration: TableEntry['configuration']
): string | null => {
  const customColumns = tableConfiguration?.custom_column_names ?? {};
  const sortClausesArr = sorts.map((i: OrderBy) => {
    return `${customColumns[i.column] ?? i.column}: ${i.type}`;
  });
  return sortClausesArr.length
    ? `order_by: {${sortClausesArr.join(',')}}`
    : null;
};

const getColQuery = (
  cols: (string | { name: string; columns: string[] })[],
  limit: number,
  relationships: Relationship[],
  tableConfiguration: TableEntry['configuration']
): string[] => {
  return cols.map(c => {
    const customColumns = tableConfiguration?.custom_column_names ?? {};
    if (typeof c === 'string') return customColumns[c] ?? c;
    const rel = relationships.find((r: any) => r.rel_name === c.name);
    return `${customColumns[c.name] ?? c.name} ${
      rel?.rel_type === 'array' ? `(limit: ${limit})` : ''
    } { 
      ${getColQuery(c.columns, limit, relationships, tableConfiguration).join(
        '\n'
      )} }`;
  });
};

export const getGraphQLQueryForBrowseRows = ({
  allSchemas,
  view,
  originalTable,
  currentSchema,
  isExport = false,
  tableConfiguration,
}: GetGraphQLQuery) => {
  const currentTable: Table | undefined = allSchemas?.find(
    (t: Table) =>
      t.table_name === originalTable && t.table_schema === currentSchema
  );
  const columnTypeInfo: BaseTableColumn[] = currentTable?.columns || [];
  const relationshipInfo: Relationship[] = currentTable?.relationships || [];

  if (!columnTypeInfo) {
    throw new Error('Error in finding column info for table');
  }

  let whereConditions: WhereClause[] = [];
  let isRelationshipView = false;
  if (view.query.where) {
    if (view.query.where.$and) {
      whereConditions = view.query.where.$and;
    } else {
      isRelationshipView = true;
      whereConditions = Object.keys(view.query.where)
        .filter(k => view.query.where[k])
        .map(k => {
          const obj = {} as any;
          obj[k] = { $eq: view.query.where[k] };
          return obj;
        });
    }
  }
  const sortConditions: OrderBy[] = [];
  if (view.query.order_by) {
    sortConditions.push(...view.query.order_by);
  }
  const limit = isExport ? null : `limit: ${view.curFilter.limit}`;
  const offset = isExport
    ? null
    : `offset: ${!isRelationshipView ? view.curFilter.offset : 0}`;
  const clauses = `${[
    generateWhereClauseQueryString(
      whereConditions,
      columnTypeInfo,
      tableConfiguration
    ),
    generateSortClauseQueryString(sortConditions, tableConfiguration),
    limit,
    offset,
  ]
    .filter(Boolean)
    .join(',')}`;

  return `query TableRows {
      ${
        currentSchema === 'dbo'
          ? originalTable
          : `${currentSchema}_${originalTable}`
      } ${clauses && `(${clauses})`} {
          ${getColQuery(
            view.query.columns,
            view.curFilter.limit,
            relationshipInfo,
            tableConfiguration
          ).join('\n')} 
    }
  }`;
};

export const getTableRowRequestBody = ({
  tables,
  isExport,
  tableConfiguration,
}: {
  tables: Tables;
  isExport?: boolean;
  tableConfiguration?: TableEntry['configuration'];
}) => {
  // TODO: fetch count when agregation for mssql is added
  const {
    currentTable: originalTable,
    view,
    allSchemas,
    currentSchema,
  } = tables;

  return {
    query: getGraphQLQueryForBrowseRows({
      allSchemas,
      view,
      originalTable,
      currentSchema,
      isExport,
      tableConfiguration,
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
    tableConfiguration?: TableEntry['configuration'];
  }
) => {
  const { originalTable, currentSchema } = config!;

  const reversedCustomColumns = Object.entries(
    config?.tableConfiguration?.custom_column_names ?? {}
  ).reduce((acc: Record<string, string>, [col, customCol]) => {
    acc[customCol] = col;
    return acc;
  }, {});

  const results =
    data.data[
      currentSchema === 'dbo'
        ? originalTable
        : `${currentSchema}_${originalTable}`
    ];

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
  return { estimatedCount: rows.length, rows };
};

export const generateTableRowRequest = () => {
  return {
    endpoint: Endpoints.graphQLUrl,
    getTableRowRequestBody,
    processTableRowData,
  };
};
