import { formatSdl } from 'format-graphql';
import { CustomRootFields, TableConfig } from './../../metadata/types';
import {
  OrderBy,
  WhereClauseRecord,
} from '../../components/Common/utils/v1QueryUtils';
import { ReduxState } from '../../types';
import { BaseTableColumn, Relationship, Table } from '../types';
import { SourceCustomization } from '../../features/hasura-metadata-types';
import { getQueryName } from './utils';

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
  dataSourceCustomization: SourceCustomization;
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
  tableConfiguration: TableConfig,
  dataSourceCustomization: SourceCustomization
): string[] => {
  return cols.map(column => {
    const columnConfig = tableConfiguration?.column_config ?? {};
    if (typeof column === 'string')
      return columnConfig[column]?.custom_name ?? column;

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

export const operators = [
  { name: 'equals', value: '$eq', graphqlOp: '_eq' },
  { name: 'not equals', value: '$ne', graphqlOp: '_neq' },
  { name: '>', value: '$gt', graphqlOp: '_gt' },
  { name: '<', value: '$lt', graphqlOp: '_lt' },
  { name: '>=', value: '$gte', graphqlOp: '_gte' },
  { name: '<=', value: '$lte', graphqlOp: '_lte' },
];

export const generateWhereClauseQueryString = (
  wheres: WhereClauseRecord[],
  columnTypeInfo: BaseTableColumn[],
  tableConfiguration: TableConfig,
  getFormattedValue: (type: string, value: any) => string | number | undefined
): string | null => {
  const columnConfig = tableConfiguration?.column_config ?? {};
  const whereClausesArr = wheres.map(whereClause => {
    const columnName = Object.keys(whereClause)[0];
    const rqlOperator = Object.keys(whereClause[columnName])[0];
    const value = whereClause[columnName][rqlOperator];
    const currentColumnInfo = columnTypeInfo?.find(
      c => c.column_name === columnName
    );

    // NOTE: the usage of `data_type` has been introduced as a workaround to support BigQuery, for which `data_type_name` is not defined here
    const columnType =
      currentColumnInfo?.data_type_name || currentColumnInfo?.data_type;

    const queryColumnName = columnConfig[columnName]?.custom_name ?? columnName;
    const operator = rqlOperator;
    const formattedValue = getFormattedValue(columnType || 'varchar', value);

    return `${queryColumnName}: {${operator}: ${formattedValue} }`;
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

  let whereConditions: WhereClauseRecord[] = [];
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

export const getFullQueryNameBase =
  (defaultSchema: string) =>
  ({
    tableName,
    schema,
    tableConfiguration,
    dataSourceCustomization,
    operation,
  }: GetFullQueryName): string => {
    const prefix = dataSourceCustomization?.root_fields?.prefix ?? '';
    const suffix = dataSourceCustomization?.root_fields?.suffix ?? '';

    const customRootFields = tableConfiguration?.custom_root_fields ?? {};
    const customRootField = customRootFields[operation];

    const withUpdate =
      operation === 'update' && !customRootFields?.update ? 'update_' : '';
    const withSchema =
      schema === defaultSchema || tableConfiguration?.custom_name
        ? ''
        : `${schema}_`;
    const withAgg =
      operation === 'select_aggregate' && !customRootFields?.select_aggregate
        ? `_aggregate`
        : '';
    const withDelete =
      operation === 'delete' && !customRootFields?.delete ? 'delete_' : '';
    const withInsert =
      operation === 'insert' && !customRootFields?.insert ? 'insert_' : '';

    const trackedTableName =
      customRootField || tableConfiguration?.custom_name || tableName;

    return `${prefix}${withDelete}${withUpdate}${withInsert}${withSchema}${trackedTableName}${withAgg}${suffix}`;
  };

type GetQueryWithNamespaceArgs = {
  queryName: string;
  namespace: string;
  innerQuery: string;
};

export const getQueryWithNamespace = ({
  queryName,
  namespace,
  innerQuery,
}: GetQueryWithNamespaceArgs) => {
  return formatSdl(`${queryName}
  {
    ${namespace ? `${namespace} {` : ''}

    ${innerQuery}

    ${namespace ? `}` : ''}
  }`);
};
