import { camelize } from 'inflection';
import {
  isScalarType,
  isEnumType,
  isInputObjectType,
  isObjectType,
  parse as sdlParse,
  validate,
  OperationDefinitionNode,
  GraphQLSchema,
  print,
  visit,
  BREAK,
  DocumentNode,
  Kind,
  GraphQLObjectType,
  SelectionNode,
} from 'graphql';
import {
  wrapTypename,
  getAstTypeMetadata,
} from '../../../../shared/utils/wrappingTypeUtils';
import {
  getTypeFields,
  getOperationType,
  collectFields,
  getFieldSelection,
} from '../../../../shared/utils/graphqlSchemaUtils';
import { isValidOperationName } from '../../../../shared/utils/sdlUtils';
import {
  inbuiltTypes,
  FlattenCustomType,
} from '../../../../shared/utils/hasuraCustomTypeUtils';
import { getGraphQLUnderlyingType } from '@hasura/shared/utils';
import { Capabilities } from '@hasura/dc-api-types';

type GraphQLType = {
  name: string;
  type: string;
};

const validateOperation = (
  operation: OperationDefinitionNode,
  clientSchema: GraphQLSchema,
) => {
  if (!isValidOperationName(operation.operation)) {
    throw Error('Subscriptions cannot be derived into actions');
  }

  if (!operation.selectionSet.selections.length) {
    throw Error('The GraphQL operation is empty');
  }

  // only the operation itself is derived, so the definitions of named
  // fragments are not available; check this before validating the operation,
  // which would otherwise fail with a generic error
  let hasFragmentSpread = false;
  visit(operation, {
    FragmentSpread: () => {
      hasFragmentSpread = true;
      return BREAK;
    },
  });
  if (hasFragmentSpread) {
    throw Error('named fragments are not supported, use inline fragments');
  }

  // parse operation string
  let operationAst: DocumentNode;
  try {
    operationAst = sdlParse(print(operation));
  } catch (e) {
    throw Object.assign(Error('this seems to be an invalid GraphQL query'), {
      cause: e,
    });
  }

  const schemaValidationErrors = validate(clientSchema, operationAst);
  if (schemaValidationErrors.length) {
    throw Error(
      'this is not a valid GraphQL query as per the current GraphQL schema',
    );
  }

  return operationAst;
};

// Action custom types can only reference the built-in GraphQL scalars and the
// scalars of Postgres-family sources (see Note [Postgres scalars in custom
// types] in the server). Returns the built-in scalar to use in place of each
// scalar declared by a data connector, e.g. MongoDB's `ObjectId`: the
// `graphql_type` the connector parses it as, or `String` otherwise.
export const getDataConnectorScalarReplacements = (
  allCapabilities: (Capabilities | undefined)[],
): Record<string, string> =>
  Object.fromEntries(
    allCapabilities.flatMap((capabilities) =>
      Object.entries(capabilities?.scalar_types ?? {})
        .filter(([name]) => !inbuiltTypes[name])
        .map(([name, scalarType]) => [
          name,
          scalarType.graphql_type ?? 'String',
        ]),
    ),
  );

const deriveAction = (
  operation: OperationDefinitionNode,
  clientSchema: GraphQLSchema,
  actionName: string | null = null,
  // see `getDataConnectorScalarReplacements`
  scalarReplacements: Record<string, string> = {},
) => {
  if (operation.selectionSet.selections.length > 1) {
    throw Error('You can derive action from only one root query or mutation');
  }

  validateOperation(operation, clientSchema);

  const variables = operation.variableDefinitions ?? [];

  // get operation name
  const rootSelection = operation.selectionSet.selections[0];
  if (rootSelection.kind !== Kind.FIELD) {
    throw Error('the root of the operation must be a field, not a fragment');
  }
  const rootFields = collectFields(
    rootSelection.selectionSet?.selections ?? [],
  ).filter((s) => s.name.value.indexOf('__') !== 0);
  const operationName = operation.name?.value;
  const selectedFields = rootFields.map((s) => s.name.value);

  // throw error if no operation is being made
  if (!selectedFields.length) {
    throw Error('the given operation must ask for at least one root field');
  }

  // get action name if not provided
  if (!actionName) {
    actionName = operationName || camelize(`${operationName}_derived`);
  }

  // function to prefix typename with the action name
  const prefixTypename = (typename) => {
    return camelize(`${actionName}_${typename}`);
  };

  const allHasuraTypes = clientSchema.getTypeMap();
  const operationType = getOperationType(clientSchema, operation.operation);

  const isHasuraScalar = (name) => {
    return isScalarType(allHasuraTypes[name]);
  };

  const toActionScalar = (name: string) => scalarReplacements[name] ?? name;

  const actionArguments: GraphQLType[] = [];
  // the types are returned in the shape of the custom types metadata, as
  // expected by `getTypesSdl`
  const newTypes: Record<string, FlattenCustomType> = {};
  const handledTypenames = new Set<string>();

  const handleType = (type, typename: string) => {
    if (handledTypenames.has(typename)) {
      return;
    }
    handledTypenames.add(typename);

    if (isScalarType(type)) {
      if (!inbuiltTypes[type.name] && !allHasuraTypes[type.name]) {
        newTypes[typename] = {
          kind: 'scalars',
          definition: { name: typename },
        };
      }
      return;
    }

    if (isEnumType(type)) {
      newTypes[typename] = {
        kind: 'enums',
        definition: {
          name: typename,
          values: type.getValues().map((v) => ({
            value: v.name,
            description: v.description ?? undefined,
          })),
        },
      };
      return;
    }

    if (isInputObjectType(type)) {
      const fields: GraphQLType[] = [];
      const typeFields = getTypeFields(type);
      Object.values(typeFields).forEach((tf) => {
        const { type: underLyingType, wraps: fieldTypeWraps } =
          getGraphQLUnderlyingType(tf.type);

        const _tf = {
          name: tf.name,
          type:
            inbuiltTypes[underLyingType.name] ||
            isHasuraScalar(underLyingType.name)
              ? wrapTypename(
                  toActionScalar(underLyingType.name),
                  fieldTypeWraps,
                )
              : wrapTypename(
                  prefixTypename(underLyingType.name),
                  fieldTypeWraps,
                ),
        };

        handleType(underLyingType, prefixTypename(underLyingType.name));
        fields.push(_tf);
      });
      newTypes[typename] = {
        kind: 'input_objects',
        definition: { name: typename, fields },
      };
    }
  };

  variables.forEach((v) => {
    const generatedArg = {
      name: v.variable.name.value,
      type: '',
    };

    const argTypeMetadata = getAstTypeMetadata(v.type);
    if (
      !inbuiltTypes[argTypeMetadata.typename] &&
      !isHasuraScalar(argTypeMetadata.typename)
    ) {
      const argTypename = prefixTypename(argTypeMetadata.typename);
      generatedArg.type = wrapTypename(argTypename, argTypeMetadata.stack);
      const typeInSchema = allHasuraTypes[argTypeMetadata.typename];
      handleType(typeInSchema, argTypename);
    } else {
      generatedArg.type = wrapTypename(
        toActionScalar(argTypeMetadata.typename),
        argTypeMetadata.stack,
      );
    }
    actionArguments.push(generatedArg);
  });

  // Adds the action object type `typename`, with the selected fields of the
  // schema object type. The selections of nested objects, e.g. the
  // `returning` of insert mutations, become nested object types. Returns
  // false if none of the selected fields could be mapped.
  const handleOutputType = (
    objectType: GraphQLObjectType,
    selections: readonly SelectionNode[],
    typename: string,
  ): boolean => {
    const fields: GraphQLType[] = [];
    Object.values(objectType.getFields()).forEach((field) => {
      const selection = getFieldSelection(selections, field.name);
      if (!selection) {
        return;
      }

      const { type, wraps } = getGraphQLUnderlyingType(field.type);
      if (isScalarType(type)) {
        fields.push({
          name: field.name,
          type: wrapTypename(toActionScalar(type.name), wraps),
        });
      } else if (isEnumType(type)) {
        const enumTypename = prefixTypename(type.name);
        handleType(type, enumTypename);
        fields.push({
          name: field.name,
          type: wrapTypename(enumTypename, wraps),
        });
      } else if (isObjectType(type)) {
        const nestedTypename = camelize(`${typename}_${field.name}`);
        if (handleOutputType(type, selection.selections, nestedTypename)) {
          fields.push({
            name: field.name,
            type: wrapTypename(nestedTypename, wraps),
          });
        }
      }
    });

    if (!fields.length) {
      return false;
    }
    newTypes[typename] = {
      kind: 'objects',
      definition: { name: typename, fields },
    };
    return true;
  };

  const actionOutputTypename = prefixTypename('output');

  const rfName = rootSelection.name.value;
  const fields = operationType?.getFields();
  if (!fields?.[rfName]) {
    throw new Error(`root field ${rfName} does not exist in GraphQL schema`);
  }

  const { type: refOperationOutputType, wraps: refOperationOutputWraps } =
    getGraphQLUnderlyingType(fields[rfName].type);
  // the root field returns an object type (e.g. `user`), whose selected
  // fields become the fields of the action output type
  if (
    !isObjectType(refOperationOutputType) ||
    !handleOutputType(
      refOperationOutputType,
      rootSelection.selectionSet?.selections ?? [],
      actionOutputTypename,
    )
  ) {
    throw new Error(
      `no field of your operation can be mapped onto the output type of the derived action; only scalar, enum and object fields are supported`,
    );
  }

  return {
    types: Object.values(newTypes),
    action: {
      name: actionName,
      type: operation.operation,
      arguments: actionArguments,
      // keep the list and non-null wrappers of the root field, e.g. a root
      // field returning `[user!]!` derives an action returning `[Output!]!`
      output_type: wrapTypename(actionOutputTypename, refOperationOutputWraps),
    },
    variables,
  };
};

export default deriveAction;
