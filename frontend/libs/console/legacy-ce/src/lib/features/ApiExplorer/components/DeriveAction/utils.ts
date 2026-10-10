import { camelize } from 'inflection';
import {
  isScalarType,
  isEnumType,
  isInputObjectType,
  parse as sdlParse,
  validate,
  OperationDefinitionNode,
  GraphQLSchema,
  print,
  DocumentNode,
  Kind,
  FieldNode,
} from 'graphql';
import {
  wrapTypename,
  getAstTypeMetadata,
} from '../../../../shared/utils/wrappingTypeUtils';
import {
  getTypeFields,
  getOperationType,
} from '../../../../shared/utils/graphqlSchemaUtils';
import { isValidOperationName } from '../../../../shared/utils/sdlUtils';
import { inbuiltTypes } from '../../../../shared/utils/hasuraCustomTypeUtils';
import { getGraphQLUnderlyingType } from '@hasura/shared/utils';

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

  // parse operation string
  let operationAst: DocumentNode;
  try {
    operationAst = sdlParse(print(operation));
  } catch (e) {
    throw Error('this seems to be an invalid GraphQL query');
  }

  const schemaValidationErrors = validate(clientSchema, operationAst);
  if (schemaValidationErrors.length) {
    throw Error(
      'this is not a valid GraphQL query as per the current GraphQL schema',
    );
  }

  return operationAst;
};

const deriveAction = (
  operation: OperationDefinitionNode,
  clientSchema: GraphQLSchema,
  actionName: string | null = null,
) => {
  if (operation.selectionSet.selections.length > 1) {
    throw Error('You can derive action from only one root query or mutation');
  }

  const operationAst = validateOperation(operation, clientSchema);

  if (operationAst.definitions.some((d) => d.kind === 'FragmentDefinition')) {
    throw Error('fragments are not supported');
  }

  const variables = operation.variableDefinitions ?? [];

  // get operation name
  const rootSelection = operation.selectionSet.selections[0] as FieldNode;
  const rootFields = rootSelection.selectionSet?.selections.filter((s) => {
    return s.kind === Kind.FIELD && s.name.value.indexOf('__') !== 0;
  }) as FieldNode[];
  const operationName = operation.name?.value;
  const selectedFields = rootFields.map((s) => {
    return (s as FieldNode).name.value;
  });

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

  const actionArguments: GraphQLType[] = [];
  const newTypes: Record<string, any> = {};

  const handleType = (type, typename) => {
    if (newTypes[typename]) {
      return;
    }
    const newType: Record<string, any> = {
      name: typename,
    };

    if (isScalarType(type)) {
      if (!inbuiltTypes[type.name] && !allHasuraTypes[type.name]) {
        newType.kind = 'scalar';
        newTypes[typename] = newType;
      }
      return;
    }

    if (isEnumType(type)) {
      newType.kind = 'enum';
      newType.values = type.getValues().map((v) => ({
        value: v.value,
        description: v.description,
      }));
      newTypes[typename] = newType;
      return;
    }

    if (isInputObjectType(type)) {
      newType.kind = 'input_object';
      newType.fields = [];
      const typeFields = getTypeFields(type);
      newTypes[typename] = true;
      Object.values(typeFields).forEach((tf) => {
        const { type: underLyingType, wraps: fieldTypeWraps } =
          getGraphQLUnderlyingType(tf.type);

        const _tf = {
          name: tf.name,
          type:
            inbuiltTypes[underLyingType.name] ||
            isHasuraScalar(underLyingType.name)
              ? wrapTypename(underLyingType.name, fieldTypeWraps)
              : wrapTypename(
                  prefixTypename(underLyingType.name),
                  fieldTypeWraps,
                ),
        };

        handleType(underLyingType, prefixTypename(underLyingType.name));
        newType.fields.push(_tf);
      });
      newTypes[typename] = newType;
      return;
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
        argTypeMetadata.typename,
        argTypeMetadata.stack,
      );
    }
    actionArguments.push(generatedArg);
  });

  const actionOutputTypename = prefixTypename('output');
  const actionOutputType = {
    name: actionOutputTypename,
    kind: 'object',
    fields: [] as GraphQLType[],
  };

  const outputTypeFields = {};
  const rfName = rootSelection.name.value;
  const fields = operationType?.getFields();
  if (!fields?.[rfName]) {
    throw new Error(`root field ${rfName} does not exist in GraphQL schema`);
  }

  const refOperationOutputType = getGraphQLUnderlyingType(
    fields[rfName].type,
  ).type;
  if (isInputObjectType(refOperationOutputType)) {
    Object.values(getTypeFields(refOperationOutputType)).forEach(
      (outputTypeField) => {
        const fieldTypeMetadata = getGraphQLUnderlyingType(
          outputTypeField.type,
        );
        if (
          isScalarType(fieldTypeMetadata.type) &&
          selectedFields.includes(outputTypeField.name)
        ) {
          outputTypeFields[outputTypeField.name] = wrapTypename(
            fieldTypeMetadata.type.name,
            fieldTypeMetadata.wraps,
          );
        }
      },
    );
  }

  if (!Object.keys(outputTypeFields).length) {
    throw new Error(
      `no scalar found in the selection set of your operation; only scalar fields of the operation get mapped onto the output type of the derived action`,
    );
  }

  actionOutputType.fields = Object.keys(outputTypeFields).map((fieldName) => {
    return {
      name: fieldName,
      type: outputTypeFields[fieldName],
    };
  });

  newTypes[actionOutputTypename] = actionOutputType;

  return {
    types: Object.values(newTypes),
    action: {
      name: actionName,
      type: operation.operation,
      arguments: actionArguments,
      output_type: actionOutputTypename,
    },
    variables,
  };
};

export default deriveAction;
