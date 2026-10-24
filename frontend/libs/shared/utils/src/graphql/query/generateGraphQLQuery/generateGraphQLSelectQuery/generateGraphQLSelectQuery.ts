import {
  getNamedType,
  GraphQLInputFieldMap,
  GraphQLSchema,
  isEnumType,
  isInputObjectType,
  isObjectType,
} from 'graphql';
import { MetadataTable, Source, OrderBy, Where } from '@hasura/shared/types';
import { getQueryRoot } from '../../getQueryRoot';
import { findRootField, getRootFieldMap } from '../../getRootField';
import {
  createBoolExpConverter,
  createColumnFieldResolver,
  getArg,
  toGraphQLInputValue,
} from '../../../common/tableFields';

export type GraphQLSelectQuery = {
  query: string;
  variables: Record<string, unknown>;
  /** path of the rows in the response `data` */
  resultPath: string[];
  /** maps each selected GraphQL field back to its table column name */
  columnNames: Record<string, string>;
};

/**
 * Generates the GraphQL query that selects the rows of a table.
 *
 * Names are looked up in the GraphQL schema rather than guessed, so naming
 * conventions and customizations are honoured, and the `where`/`order_by`
 * values are sent as variables. Returns `undefined` when the schema doesn't
 * expose a select field for the table.
 */
export const generateGraphQLSelectQuery = ({
  schema,
  defaultQueryRoot,
  columns,
  tableCustomization,
  sourceCustomization,
  options,
  operationName = 'TableRows',
}: {
  schema: GraphQLSchema;
  operationName?: string;
  defaultQueryRoot: string;
  /** table column names */
  columns: string[];
  tableCustomization: MetadataTable['configuration'];
  sourceCustomization: Source['customization'];
  options?: {
    where?: Where | Where[];
    offset?: number;
    limit?: number;
    order_by?: OrderBy[];
  };
}): GraphQLSelectQuery | undefined => {
  const namespace = sourceCustomization?.root_fields?.namespace || undefined;
  const rootField = findRootField(
    getRootFieldMap(schema.getQueryType(), namespace),
    getQueryRoot({
      defaultQueryRoot,
      operation: 'select',
      configuration: tableCustomization,
      sourceCustomization,
    }),
  );
  const rowType = rootField && getNamedType(rootField.type);
  if (!rootField || !isObjectType(rowType)) return undefined;

  const getFieldName = createColumnFieldResolver(
    rowType.getFields(),
    tableCustomization,
  );

  const columnNames: Record<string, string> = {};
  columns.forEach((column) => {
    const fieldName = getFieldName(column);
    if (fieldName) columnNames[fieldName] = column;
  });

  const selection = Object.keys(columnNames);
  if (!selection.length) return undefined;

  const toBoolExp = createBoolExpConverter(getFieldName);

  const orderByArg = getArg(rootField, 'order_by');
  const orderByType = orderByArg && getNamedType(orderByArg.type);
  const orderByFields: GraphQLInputFieldMap = isInputObjectType(orderByType)
    ? orderByType.getFields()
    : {};

  // the order_by enum values are upper-cased by the `graphql-default` naming
  // convention, so match them case-insensitively
  const toOrderBy = ({ column, type, nulls }: OrderBy) => {
    const fieldName = getFieldName(column) ?? column;
    const direction = nulls ? `${type}_nulls_${nulls}` : type;
    const enumType =
      orderByFields[fieldName] && getNamedType(orderByFields[fieldName].type);
    const value = isEnumType(enumType)
      ? enumType
          .getValues()
          .find((v) => v.name.toLowerCase() === direction.toLowerCase())?.name
      : undefined;

    return { [fieldName]: value ?? direction };
  };

  const variables: Record<string, unknown> = {};
  const variableDefinitions: string[] = [];
  const args: string[] = [];
  const addArg = (name: string, value: unknown) => {
    const arg = getArg(rootField, name);
    if (!arg || value === undefined) return;

    variables[arg.name] = toGraphQLInputValue(arg.type, value);
    variableDefinitions.push(`$${arg.name}: ${arg.type.toString()}`);
    args.push(`${arg.name}: $${arg.name}`);
  };

  addArg('where', options?.where ? toBoolExp(options.where) : undefined);
  addArg(
    'order_by',
    options?.order_by?.length ? options.order_by.map(toOrderBy) : undefined,
  );
  addArg('limit', options?.limit || undefined);
  addArg('offset', options?.offset || undefined);

  const select = `${rootField.name}${args.length ? `(${args.join(', ')})` : ''} {
    ${selection.join('\n    ')}
  }`;

  return {
    query: `query ${operationName}${
      variableDefinitions.length ? `(${variableDefinitions.join(', ')})` : ''
    } {
  ${namespace ? `${namespace} { ${select} }` : select}
}`,
    variables,
    resultPath: namespace ? [namespace, rootField.name] : [rootField.name],
    columnNames,
  };
};
