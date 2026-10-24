import {
  getNamedType,
  GraphQLField,
  GraphQLSchema,
  isInputObjectType,
  isObjectType,
} from 'graphql';
import { camelize } from 'inflection';
import { MetadataTable, Source, Where } from '@hasura/shared/types';
import { findRootField, getRootFieldMap } from '../query/getRootField';
import {
  createBoolExpConverter,
  createColumnFieldResolver,
  getArg,
  toGraphQLInputValue,
} from '../common/tableFields';
import { getMutationRoot } from './getMutationRoot';

type Row = Record<string, unknown>;

export type GraphQLRowMutationOperation = 'insert' | 'update' | 'delete';

export type GraphQLRowMutation = {
  query: string;
  variables: Record<string, unknown>;
  /** path of the mutation response in the response `data` */
  resultPath: string[];
  /** response field holding the number of affected rows */
  affectedRowsField: string;
  /** response field holding the returned rows, when columns were requested */
  returningField?: string;
  /** maps each returned GraphQL field back to its table column name */
  columnNames: Record<string, string>;
};

const findField = (
  fields: Record<string, GraphQLField<unknown, unknown>>,
  name: string,
) => fields[name] ?? fields[camelize(name, true)];

/**
 * Generates the GraphQL mutation that inserts, updates or deletes the rows of
 * a table.
 *
 * Like `generateGraphQLSelectQuery`, names are looked up in the GraphQL schema
 * rather than guessed, so naming conventions and customizations are honoured,
 * and the values are sent as variables. `objects`, `set` and `where` are keyed
 * by table column names. Returns `undefined` when the schema doesn't expose
 * the mutation for the table.
 */
export const generateGraphQLRowMutation = ({
  schema,
  defaultQueryRoot,
  operation,
  tableCustomization,
  sourceCustomization,
  objects,
  set,
  where,
  returning = [],
  operationName = 'TableRowsMutation',
}: {
  schema: GraphQLSchema;
  defaultQueryRoot: string;
  operation: GraphQLRowMutationOperation;
  tableCustomization: MetadataTable['configuration'];
  sourceCustomization: Source['customization'];
  /** rows to insert */
  objects?: Row[];
  /** values to update */
  set?: Row;
  /** rows to update or delete */
  where?: Where | Where[];
  /** table column names to return */
  returning?: string[];
  operationName?: string;
}): GraphQLRowMutation | undefined => {
  const namespace = sourceCustomization?.root_fields?.namespace || undefined;
  const rootField = findRootField(
    getRootFieldMap(schema.getMutationType(), namespace),
    getMutationRoot({
      defaultQueryRoot,
      operation,
      configuration: tableCustomization,
      sourceCustomization,
    }),
  );
  const responseType = rootField && getNamedType(rootField.type);
  if (!rootField || !isObjectType(responseType)) return undefined;

  const responseFields = responseType.getFields();
  const affectedRowsField = findField(responseFields, 'affected_rows');
  const returningField = findField(responseFields, 'returning');
  const rowType = returningField && getNamedType(returningField.type);
  if (!affectedRowsField || !isObjectType(rowType)) return undefined;

  const getFieldName = createColumnFieldResolver(
    rowType.getFields(),
    tableCustomization,
  );

  // keys that aren't exposed are sent as-is so the server reports them
  const toInput = (argName: string, row: Row) => {
    const arg = getArg(rootField, argName);
    const inputType = arg && getNamedType(arg.type);
    const getInputFieldName = isInputObjectType(inputType)
      ? createColumnFieldResolver(inputType.getFields(), tableCustomization)
      : getFieldName;

    return Object.fromEntries(
      Object.entries(row).map(([column, value]) => [
        getInputFieldName(column) ?? column,
        value,
      ]),
    );
  };

  const columnNames: Record<string, string> = {};
  returning.forEach((column) => {
    const fieldName = getFieldName(column);
    if (fieldName) columnNames[fieldName] = column;
  });

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

  if (operation === 'insert')
    addArg(
      'objects',
      objects?.map((row) => toInput('objects', row)),
    );
  if (operation === 'update') addArg('_set', set && toInput('_set', set));
  if (operation !== 'insert')
    addArg('where', where && createBoolExpConverter(getFieldName)(where));

  const returningSelection = Object.keys(columnNames).length
    ? `${returningField.name} { ${Object.keys(columnNames).join(' ')} }`
    : '';
  const mutation = `${rootField.name}${
    args.length ? `(${args.join(', ')})` : ''
  } { ${affectedRowsField.name} ${returningSelection} }`;

  return {
    query: `mutation ${operationName}${
      variableDefinitions.length ? `(${variableDefinitions.join(', ')})` : ''
    } {
  ${namespace ? `${namespace} { ${mutation} }` : mutation}
}`,
    variables,
    resultPath: namespace ? [namespace, rootField.name] : [rootField.name],
    affectedRowsField: affectedRowsField.name,
    returningField: returningSelection ? returningField.name : undefined,
    columnNames,
  };
};
