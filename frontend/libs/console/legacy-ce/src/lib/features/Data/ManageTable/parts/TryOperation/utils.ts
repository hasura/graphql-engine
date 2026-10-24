import {
  getNamedType,
  GraphQLField,
  GraphQLSchema,
  isLeafType,
  isObjectType,
  isInputObjectType,
  isRequiredArgument,
} from 'graphql';
import { camelize } from 'inflection';
import type { TableColumn } from '@hasura/metadata/data-source';
import type { MetadataTable, Source } from '@hasura/shared/types';
import {
  capitalizeFirstLetter,
  findRootField,
  formatGraphQL,
  getMutationRoot,
  getQueryRoot,
  getRootFieldMap,
} from '@hasura/shared/utils';

export type OperationType =
  'query' | 'mutation' | 'subscription' | 'streaming_subscription';

export type TableRootFields = {
  namespace?: string;
  query?: GraphQLField<unknown, unknown>;
  insert?: GraphQLField<unknown, unknown>;
  subscription?: GraphQLField<unknown, unknown>;
  stream?: GraphQLField<unknown, unknown>;
};

const getStreamRoot = ({
  defaultQueryRoot,
  configuration,
  sourceCustomization,
}: {
  defaultQueryRoot: string;
  configuration?: MetadataTable['configuration'];
  sourceCustomization?: Source['customization'];
}) => {
  const base =
    configuration?.custom_root_fields?.select_stream ??
    `${configuration?.custom_name ?? defaultQueryRoot}_stream`;
  return `${sourceCustomization?.root_fields?.prefix ?? ''}${base}${
    sourceCustomization?.root_fields?.suffix ?? ''
  }`;
};

/**
 * Resolves the root fields generated for a table by looking them up in the
 * live GraphQL schema. Operations that aren't exposed (driver capabilities,
 * permissions, disabled root fields...) are left undefined.
 */
export const getTableRootFields = ({
  schema,
  defaultQueryRoot,
  configuration,
  sourceCustomization,
}: {
  schema: GraphQLSchema;
  defaultQueryRoot: string;
  configuration?: MetadataTable['configuration'];
  sourceCustomization?: Source['customization'];
}): TableRootFields => {
  const namespace = sourceCustomization?.root_fields?.namespace || undefined;
  const queryFields = getRootFieldMap(schema.getQueryType(), namespace);
  const mutationFields = getRootFieldMap(schema.getMutationType(), namespace);
  const subscriptionFields = getRootFieldMap(
    schema.getSubscriptionType(),
    namespace,
  );

  const selectRoot = getQueryRoot({
    defaultQueryRoot,
    operation: 'select',
    configuration,
    sourceCustomization,
  });
  const insertRoot = getMutationRoot({
    defaultQueryRoot,
    operation: 'insert',
    configuration,
    sourceCustomization,
  });
  const streamRoot = getStreamRoot({
    defaultQueryRoot,
    configuration,
    sourceCustomization,
  });

  return {
    namespace,
    query: findRootField(queryFields, selectRoot),
    insert: findRootField(mutationFields, insertRoot),
    subscription: findRootField(subscriptionFields, selectRoot),
    stream: findRootField(subscriptionFields, streamRoot),
  };
};

/** Scalar/enum fields of the field's return type that need no arguments. */
const getSelection = (field: GraphQLField<unknown, unknown>): string[] => {
  const type = getNamedType(field.type);
  if (!isObjectType(type)) return [];

  return Object.values(type.getFields())
    .filter(
      (f) =>
        isLeafType(getNamedType(f.type)) && !f.args.some(isRequiredArgument),
    )
    .map((f) => f.name);
};

const getColumnGraphQLName = (
  column: TableColumn,
  configuration?: MetadataTable['configuration'],
) =>
  column.graphQLProperties?.name ??
  configuration?.column_config?.[column.name]?.custom_name ??
  column.name;

const getSampleValue = (column: TableColumn): unknown => {
  switch (column.consoleDataType) {
    case 'integer':
    case 'number':
    case 'float':
      return 0;
    case 'boolean':
      return true;
    case 'timestamp':
      return new Date().toISOString();
    case 'date':
      return new Date().toISOString().slice(0, 10);
    case 'time':
      return '00:00:00+00';
    case 'uuid':
      return '00000000-0000-0000-0000-000000000000';
    case 'json':
      return {};
    case 'array':
      return [];
    default:
      return '';
  }
};

/** Prints a JSON value as a GraphQL input literal (unquoted object keys). */
export const toGraphQLLiteral = (value: unknown): string => {
  if (Array.isArray(value))
    return `[${value.map(toGraphQLLiteral).join(', ')}]`;
  if (value !== null && typeof value === 'object') {
    return `{${Object.entries(value)
      .map(([k, v]) => `${k}: ${toGraphQLLiteral(v)}`)
      .join(', ')}}`;
  }
  return JSON.stringify(value) ?? 'null';
};

export const getInitialValueColumn = (
  columns: TableColumn[],
): TableColumn | undefined =>
  columns.find((c) => c.isPrimaryKey && c.consoleDataType === 'integer') ??
  columns.find((c) =>
    ['timestamp', 'date', 'time'].includes(c.consoleDataType),
  );

const getInputFieldNames = (
  field: GraphQLField<unknown, unknown>,
  argName: string,
): Set<string> => {
  const arg = field.args.find((a) => a.name === argName);
  const type = arg ? getNamedType(arg.type) : undefined;
  return new Set(isInputObjectType(type) ? Object.keys(type.getFields()) : []);
};

const matchInputField = (inputFields: Set<string>, name: string) =>
  [name, camelize(name, true)].find((n) => inputFields.has(n));

const toPascalCase = (str: string) =>
  str
    .split(/[-_\s.]/)
    .filter(Boolean)
    .map(capitalizeFirstLetter)
    .join('');

const wrapNamespace = (namespace: string | undefined, body: string) =>
  namespace ? `${namespace} { ${body} }` : body;

/**
 * Generates a sample GraphQL operation for a table. Returns `undefined` when
 * the operation isn't available in the schema.
 */
export const generateGqlQueryFromTable = ({
  operationType,
  tableName,
  rootFields,
  columns,
  configuration,
}: {
  operationType: OperationType;
  /** used to name the operation */
  tableName: string;
  rootFields: TableRootFields;
  columns: TableColumn[];
  configuration?: MetadataTable['configuration'];
}): string | undefined => {
  const name = toPascalCase(tableName);
  const { namespace } = rootFields;

  if (operationType === 'query' || operationType === 'subscription') {
    const field = rootFields[operationType];
    if (!field) return undefined;

    return formatGraphQL(
      `${operationType} Get${name} {
        ${wrapNamespace(namespace, `${field.name} { ${getSelection(field).join('\n')} }`)}
      }`,
    );
  }

  if (operationType === 'mutation') {
    const field = rootFields.insert;
    if (!field) return undefined;

    const inputFields = getInputFieldNames(field, 'objects');
    const object = columns
      .filter(
        (c) => c.nullable === false && !c.defaultValue && !c.value_generated,
      )
      .reduce<Record<string, unknown>>((acc, c) => {
        const inputField = matchInputField(
          inputFields,
          getColumnGraphQLName(c, configuration),
        );
        return inputField ? { ...acc, [inputField]: getSampleValue(c) } : acc;
      }, {});

    const returnType = getNamedType(field.type);
    const returning =
      isObjectType(returnType) && returnType.getFields()['returning']
        ? `returning { ${getSelection(returnType.getFields()['returning']).join('\n')} }`
        : '';

    return formatGraphQL(
      `mutation Insert${name} {
        ${wrapNamespace(
          namespace,
          `${field.name}(objects: [${toGraphQLLiteral(object)}]) {
            affected_rows
            ${returning}
          }`,
        )}
      }`,
    );
  }

  const field = rootFields.stream;
  const cursorColumn = getInitialValueColumn(columns);
  if (!field || !cursorColumn) return undefined;

  const selection = getSelection(field);
  const cursorField = matchInputField(
    new Set(selection),
    getColumnGraphQLName(cursorColumn, configuration),
  );
  if (!cursorField) return undefined;
  const cursorValue = toGraphQLLiteral(getSampleValue(cursorColumn));

  return formatGraphQL(
    `subscription Get${name}StreamingSubscription {
      ${wrapNamespace(
        namespace,
        `${field.name}(batch_size: 10, cursor: {initial_value: {${cursorField}: ${cursorValue}}}) {
          ${selection.join('\n')}
        }`,
      )}
    }`,
  );
};
