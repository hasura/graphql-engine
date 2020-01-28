import { camelize } from 'inflection';
import {
  buildClientSchema,
  isScalarType,
  isEnumType,
  isInputObjectType,
  parse as sdlParse,
} from 'graphql';
import { wrapTypename, getAstTypeMetadata } from './wrappingTypeUtils';
import { inbuiltTypes } from './hasuraCustomTypeUtils';
import { getTypeFields, getUnderlyingType } from './graphqlSchemaUtils';

export const validateMutation = mutationString => {
  // parse mutation string
  let mutationAst;
  try {
    mutationAst = sdlParse(mutationString);
  } catch (e) {
    throw Error('invalid SDL');
  }

  // throw error if the AST is empty
  if (!mutationAst.definitions.length) {
    throw Error('could not find any operation');
  }
  if (mutationAst.definitions[0].kind !== 'OperationDefinition') {
    throw Error('could not find any operation in the given mutation');
  }

  // filter schema specific fields from the operation
  mutationAst.definitions[0].selectionSet.selections = mutationAst.definitions[0].selectionSet.selections.filter(
    s => {
      return s.name.value.indexOf('__') !== 0;
    }
  );

  // throw error if no mutation is being made
  if (!mutationAst.definitions[0].selectionSet.selections.length) {
    throw Error('the given mutation must ask for one root field');
  }

  // throw error if the mutation does not have variables
  if (!mutationAst.definitions[0].variableDefinitions.length) {
    throw Error('only mutations using variables can be derived');
  }

  return mutationAst;
};

const deriveMutation = (
  mutationString,
  introspectionSchema,
  actionName = null
) => {
  let mutationAst;
  try {
    mutationAst = validateMutation(mutationString);
  } catch (e) {
    throw e;
  }

  const variables = mutationAst.definitions[0].variableDefinitions;

  // get mutation name
  const mutationDefinition =
    mutationAst.definitions[0].selectionSet.selections[0];
  const mutationName = mutationDefinition.name.value;

  // get action name if not provided
  if (!actionName) {
    actionName = mutationAst.definitions[0].name
      ? mutationAst.definitions[0].name.value
      : camelize(`${mutationName}_derived`);
  }

  // function to prefix typename with the action name
  const prefixTypename = typename => {
    return camelize(`${actionName}_${typename}`);
  };

  // parse the introspection schema
  const clientSchema = introspectionSchema.__schema
    ? buildClientSchema(introspectionSchema)
    : introspectionSchema;
  const allHasuraTypes = clientSchema._typeMap;
  const mutationType = clientSchema._mutationType;

  const actionArguments = [];
  const newTypes = {};

  const handleType = (type, typename) => {
    if (newTypes[typename]) {
      return;
    }
    const newType = {};
    newType.name = typename;

    if (isScalarType(type)) {
      if (!inbuiltTypes[type.name]) {
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
        if (inbuiltTypes[underLyingType.name]) {
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
    if (!inbuiltTypes[argTypeMetadata.typename]) {
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

  const refMutationOutputType = getUnderlyingType(
    getTypeFields(mutationType)[mutationName].type
  ).type;
  const actionOutputTypename = prefixTypename(refMutationOutputType.name);
  const actionOutputType = {
    name: actionOutputTypename,
    kind: 'object',
    fields: [],
  };

  Object.values(getTypeFields(refMutationOutputType)).forEach(
    outputTypeField => {
      const fieldTypeMetadata = getUnderlyingType(outputTypeField.type);
      if (isScalarType(fieldTypeMetadata.type)) {
        if (inbuiltTypes[fieldTypeMetadata.type.name]) {
          actionOutputType.fields.push({
            name: outputTypeField.name,
            type: wrapTypename(
              fieldTypeMetadata.type.name,
              fieldTypeMetadata.wraps
            ),
          });
        } else {
          const fieldTypename = prefixTypename(fieldTypeMetadata.type.name);
          actionOutputType.fields.push({
            name: outputTypeField.name,
            type: wrapTypename(fieldTypename, fieldTypeMetadata.wraps),
          });
          handleType(fieldTypeMetadata.type, fieldTypename);
        }
      }
    }
  );

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

export default deriveMutation;
