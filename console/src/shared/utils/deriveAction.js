import { camelize } from 'inflection';
import {
  buildClientSchema,
  isScalarType,
  isEnumType,
  isInputObjectType,
  parse as sdlParse,
  validate,
} from 'graphql';
import { wrapTypename, getAstTypeMetadata } from './wrappingTypeUtils';
import { inbuiltTypes } from './hasuraCustomTypeUtils';
import { getTypeFields, getUnderlyingType } from './graphqlSchemaUtils';

export const validateOperation = (operationString, clientSchema) => {
  // parse operation string
  let operationAst;
  try {
    operationAst = sdlParse(operationString);
  } catch (e) {
    throw Error('invalid SDL');
  }

  const schemaValidationErrors = validate(clientSchema, operationAst);
  if (schemaValidationErrors.length) {
    throw Error(
      'this is not a valid GraphQL query as per the current GraphQL schema'
    );
  }

  if (operationAst.definitions.find(d => d.kind === 'FragmentDefinition')) {
    throw Error('fragments are not supported');
  }

  if (operationAst.definitions.find(d => d.operation !== 'mutation')) {
    throw Error('queries cannot be derived into actions');
  }

  operationAst.definitions = operationAst.definitions.filter(
    d => d.operation === 'mutation'
  );

  // throw error if the AST is empty
  if (!operationAst.definitions.length) {
    throw Error('could not find any mutation operations');
  }

  if (operationAst.definitions.length !== 1) {
    throw Error('you can derive action from only one operation');
  }

  if (operationAst.definitions[0].kind !== 'OperationDefinition') {
    throw Error('could not find any operation in the given query');
  }

  // filter schema specific fields from the operation
  operationAst.definitions[0].selectionSet.selections = operationAst.definitions[0].selectionSet.selections.filter(
    s => {
      return s.name.value.indexOf('__') !== 0;
    }
  );

  // throw error if no operation is being made
  if (!operationAst.definitions[0].selectionSet.selections.length) {
    throw Error('the given operation must ask for at least one root field');
  }

  // throw error if the operation does not have variables
  if (!operationAst.definitions[0].variableDefinitions.length) {
    throw Error('only operations using variables can be derived');
  }

  return operationAst;
};

const deriveAction = (
  operationString,
  introspectionSchema,
  actionName = null
) => {
  const clientSchema = introspectionSchema.__schema
    ? buildClientSchema(introspectionSchema)
    : introspectionSchema;

  let operationAst;
  try {
    operationAst = validateOperation(operationString, clientSchema);
  } catch (e) {
    throw e;
  }

  const variables = operationAst.definitions[0].variableDefinitions;

  // get operation name
  const rootFields = operationAst.definitions[0].selectionSet.selections;
  const operationDefinition = rootFields[0];
  const operationName = operationDefinition.name.value;

  // get action name if not provided
  if (!actionName) {
    actionName = operationAst.definitions[0].name
      ? operationAst.definitions[0].name.value
      : camelize(`${operationName}_derived`);
  }

  // function to prefix typename with the action name
  const prefixTypename = typename => {
    return camelize(`${actionName}_${typename}`);
  };

  const allHasuraTypes = clientSchema._typeMap;
  const operationType = clientSchema._mutationType; // TODO better handling for queries

  const isHasuraScalar = name => {
    return isScalarType(allHasuraTypes[name]);
  };

  const actionArguments = [];
  const newTypes = {};

  const handleType = (type, typename) => {
    if (newTypes[typename]) {
      return;
    }
    const newType = {};
    newType.name = typename;

    if (isScalarType(type)) {
      if (!inbuiltTypes[type.name] && !allHasuraTypes[type.name]) {
        newType.kind = 'scalar';
        newTypes[typename] = newType;
      }
      return;
    }

    if (isEnumType(type)) {
      newType.kind = 'enum';
      newType.values = type._values.map(v => ({
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
      Object.values(typeFields).forEach(tf => {
        const _tf = { name: tf.name };
        const {
          type: underLyingType,
          wraps: fieldTypeWraps,
        } = getUnderlyingType(tf.type);
        if (
          inbuiltTypes[underLyingType.name] ||
          isHasuraScalar(underLyingType.name)
        ) {
          _tf.type = wrapTypename(underLyingType.name, fieldTypeWraps);
        } else {
          _tf.type = wrapTypename(
            prefixTypename(underLyingType.name),
            fieldTypeWraps
          );
        }
        handleType(underLyingType, prefixTypename(underLyingType.name));
        newType.fields.push(_tf);
      });
      newTypes[typename] = newType;
      return;
    }
  };

  variables.forEach(v => {
    const generatedArg = {
      name: v.variable.name.value,
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
        argTypeMetadata.stack
      );
    }
    actionArguments.push(generatedArg);
  });

  const actionOutputTypename = prefixTypename('output');
  const actionOutputType = {
    name: actionOutputTypename,
    kind: 'object',
    fields: [],
  };
  const outputTypeFields = {};
  rootFields.forEach(f => {
    const rfName = f.name.value;
    const refOperationOutputType = getUnderlyingType(
      getTypeFields(operationType)[rfName].type
    ).type;

    Object.values(getTypeFields(refOperationOutputType)).forEach(
      outputTypeField => {
        const fieldTypeMetadata = getUnderlyingType(outputTypeField.type);
        if (isScalarType(fieldTypeMetadata.type)) {
          outputTypeFields[outputTypeField.name] = wrapTypename(
            fieldTypeMetadata.type.name,
            fieldTypeMetadata.wraps
          );
        }
      }
    );
  });

  Object.keys(outputTypeFields).forEach(fieldName => {
    actionOutputType.fields.push({
      name: fieldName,
      type: outputTypeFields[fieldName],
    });
  });

  newTypes[actionOutputTypename] = actionOutputType;

  return {
    types: Object.values(newTypes),
    action: {
      name: actionName,
      arguments: actionArguments,
      output_type: actionOutputTypename,
    },
  };
};

export default deriveAction;
