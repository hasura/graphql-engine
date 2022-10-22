import { CustomRootFields, TableConfig } from './../../metadata/types';
import {
  OrderBy,
  WhereClause,
} from '../../components/Common/utils/v1QueryUtils';
import { ReduxState } from '../../types';
import { BaseTableColumn, Relationship, Table } from '../types';

type Tables = ReduxState['tables'];

export interface QueryBody {
  clauses: string;
  relationshipInfo: Relationship[];
}
export interface GetGraphQLQuery {
  allSchemas: Table[];
  view: Tables['view'];
  originalTable: string;
  currentSchema: string;
  isExport?: boolean;
  tableConfiguration: TableConfig;
  queryBody: (config: QueryBody) => string;
  getFormattedValue: (type: string, value: any) => string | number | undefined;
}
interface GetFullQueryName {
  tableName: string;
  schema: string;
  tableConfiguration: TableConfig;
  defaultSchema?: string;
  operation: keyof Omit<
    CustomRootFields,
    'select_by_pk' | 'insert_one' | 'update_by_pk' | 'delete_by_pk'
  >;
}

const generateSortClauseQueryString = (
  sorts: OrderBy[],
  tableConfiguration: TableConfig
): string | null => {
  const columnConfig = tableConfiguration?.column_config ?? {};
  const sortClausesArr = sorts.map((i: OrderBy) => {
    return `${columnConfig[i.column]?.custom_name ?? i.column}: ${i.type}`;
  });
  return sortClausesArr.length
    ? `order_by: {${sortClausesArr.join(',')}}`
    : null;
};

export const getColQuery = (
  cols: (string | { name: string; columns: string[] })[],
  limit: number,
  relationships: Relationship[],
  tableConfiguration: TableConfig
): string[] => {
  return cols.map(c => {
    const columnConfig = tableConfiguration?.column_config ?? {};
    if (typeof c === 'string') return columnConfig[c]?.custom_name ?? c;
    const rel = relationships.find((r: any) => r.rel_name === c.name);
    return `${columnConfig[c.name]?.custom_name ?? c.name} ${
      rel?.rel_type === 'array' ? `(limit: ${limit})` : ''
    } {
        ${getColQuery(c.columns, limit, relationships, tableConfiguration).join(
          '\n'
        )} }`;
  });
};

export const operators = [
  { name: 'equals', value: '$eq', graphqlOp: '_eq' },
  { name: 'not equals', value: '$ne', graphqlOp: '_neq' },
  { name: '>', value: '$gt', graphqlOp: '_gt' },
  { name: '<', value: '$lt', graphqlOp: '_lt' },
  { name: '>=', value: '$gte', graphqlOp: '_gte' },
  { name: '<=', value: '$lte', graphqlOp: '_lte' },
];

export const RqlToGraphQlOp = (op: string) => {
  if (!op || !op?.startsWith('$')) return 'none';
  return (
    operators.find(_op => _op.value === op)?.graphqlOp ?? op.replace('$', '_')
  );
};

export const generateWhereClauseQueryString = (
  wheres: WhereClause[],
  columnTypeInfo: BaseTableColumn[],
  tableConfiguration: TableConfig,
  getFormattedValue: (type: string, value: any) => string | number | undefined
): string | null => {
  const columnConfig = tableConfiguration?.column_config ?? {};
  const whereClausesArr = wheres.map((i: Record<string, any>) => {
    const columnName = Object.keys(i)[0];
    const RqlOperator = Object.keys(i[columnName])[0];
    const value = i[columnName][RqlOperator];
    const type = columnTypeInfo?.find(c => c.column_name === columnName)
      ?.data_type_name;
    return `${
      columnConfig[columnName]?.custom_name ?? columnName
    }: {${RqlToGraphQlOp(RqlOperator)}: ${getFormattedValue(
      type || 'varchar',
      value
    )} }`;
  });
  return whereClausesArr.length
    ? `where: {${whereClausesArr.join(',')}}`
    : null;
};

export const getGraphQLQueryBase = ({
  allSchemas,
  view,
  originalTable,
  currentSchema,
  isExport = false,
  tableConfiguration,
  queryBody,
  getFormattedValue,
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
  if (view.query?.where) {
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
  if (view.query?.order_by) {
    sortConditions.push(...view.query.order_by);
  }
  const limit =
    isExport || !view.curFilter?.limit
      ? null
      : `limit: ${view.curFilter.limit}`;
  const offset = isExport
    ? null
    : `offset: ${!isRelationshipView ? view.curFilter?.offset ?? 0 : 0}`;
  const clauses = `${[
    generateWhereClauseQueryString(
      whereConditions,
      columnTypeInfo,
      tableConfiguration,
      getFormattedValue
    ),
    generateSortClauseQueryString(sortConditions, tableConfiguration),
    limit,
    offset,
  ]
    .filter(Boolean)
    .join(',')}`;

  return queryBody({ clauses, relationshipInfo });
};

export const getFullQueryNameBase = (defaultSchema: string) => ({
  tableName,
  schema,
  tableConfiguration,
  operation,
}: GetFullQueryName): string => {
  const customRootFields = tableConfiguration?.custom_root_fields ?? {};
  const customRootField = customRootFields[operation];
  if (typeof customRootField === 'string') return customRootField;
  else if (customRootField?.name) return customRootField.name;
  const withUpdate = operation === 'update' ? 'update_' : '';
  const withSchema =
    schema === defaultSchema || tableConfiguration?.custom_name
      ? ''
      : `${schema}_`;
  const withAgg = operation === 'select_aggregate' ? `_aggregate` : '';
  const withDelete = operation === 'delete' ? 'delete_' : '';
  const withInsert = operation === 'insert' ? 'insert_' : '';
  const trackedTableName = tableConfiguration?.custom_name || tableName;
  return `${withDelete}${withUpdate}${withInsert}${withSchema}${trackedTableName}${withAgg}`;
};
