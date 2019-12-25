const { camelize } = require('inflection');
const { buildClientSchema, isScalarType, isEnumType, isInputObjectType } = require('graphql');
const { wrapTypename } = require('../../../shared/utils/wrappingTypeUtils')
const { inbuiltTypes } = require('../../../shared/utils/hasuraCustomTypeUtils')
const {
  getMutationType,
  getTypeFields,
  getHasuraMutationMetadata,
  getUnderlyingType,
  getFieldArgs
} = require('../../../shared/utils/graphqlSchemaUtils');

// TODO sanity

const deriveMutation = (mutationName, introspectionSchema, actionName) => {
  const clientSchema = buildClientSchema(introspectionSchema);
  const mutationType = clientSchema._mutationType;
  if (!mutationType) {
    throw Error('this mutation does not exist in the Hasura schema');
  }

  const allMutations = getTypeFields(mutationType);
  const refMutation = allMutations[mutationName];

  if (!refMutation) {
    throw Error('this mutation does not exist in the Hasura schema');
  }

  const hasuraMutationMetadata = getHasuraMutationMetadata(refMutation);
  if (!hasuraMutationMetadata) {
    throw Error('the given mutation is not a Hasura mutation');
  }

  const prefixTypename = (typename) => {
    return camelize(`${actionName}_${typename}`);
  };

  const mutationDefinition = {
    arguments: [],
    output_type: '',
    name: actionName
  };
  const newTypes = {};

  const handleType = (type, typename) => {
    if (inbuiltTypes[type.name] || inbuiltTypes[typename]) { return; }
    if (newTypes[typename]) { return; }
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
      newType.kind = 'enum',
      newType.values = type._values.map(v => ({ value: v.value, description: v.description}));
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
        const { type: underLyingType, wraps: fieldTypeWraps } = getUnderlyingType(tf.type);
        const subFields = getTypeFields(underLyingType);
        if (
          Object.keys(subFields).length === 2 &&
          Object.keys(subFields)[0] === 'data' &&
          Object.keys(subFields)[1] === 'on_conflict'
        ) {
          const { type: relType, wraps } = getUnderlyingType(Object.values(subFields)[0].type);
          _tf.type = wrapTypename(prefixTypename(relType), wraps);
          handleType(relType, prefixTypename(relType.name));
        } else {
          if (inbuiltTypes[underLyingType.name]) {
            _tf.type = wrapTypename(underLyingType.name, fieldTypeWraps);
          } else {
            _tf.type = wrapTypename(prefixTypename(underLyingType.name), fieldTypeWraps);
          }

          handleType(underLyingType, prefixTypename(underLyingType.name))
        }
        newType.fields.push(_tf);
      })
      newTypes[typename] = newType;
      return;
    }
  }

  const deriveInsertMutation = (_refMutation) => {
    const { type: mutationObjectType, wraps } = getUnderlyingType(_refMutation.args[0].type);
    mutationDefinition.arguments.push({
      name: 'input',
      type: wrapTypename(prefixTypename(mutationObjectType.name), wraps),
    });
    handleType(mutationObjectType, prefixTypename(mutationObjectType.name));
  };

  const deriveDeleteMutation = (_refMutation) => {
    const refArgs = getFieldArgs(_refMutation);
    _refMutation.args.forEach(a => {
      const _a = { name: a.name };
      const { type: argType, wraps } = getUnderlyingType(a.type);
      _a.type = wrapTypename(prefixTypename(argType.name), wraps);
      mutationDefinition.arguments.push(_a);
      handleType(argType, prefixTypename(argType.name));
    })
  }

  const deriveUpdateMutation = (_refMutation) => {
    const refArgs = getFieldArgs(_refMutation);
    _refMutation.args.forEach(a => {
      const _a = { name: a.name };
      const { type: argType, wraps } = getUnderlyingType(a.type);
      _a.type = wrapTypename(prefixTypename(argType.name), wraps);
      mutationDefinition.arguments.push(_a);
      handleType(argType, prefixTypename(argType.name));
    });
  }

  switch(hasuraMutationMetadata.kind) {
    case 'insert':
      deriveInsertMutation(refMutation);
      break;
    case 'update':
      deriveUpdateMutation(refMutation);
      break;
    case 'delete':
      deriveDeleteMutation(refMutation)
      break;
    default:
      break;
  }

  const outputTypeDef = {
    kind: 'object',
    fields: []
  };
  let { type: mutationOutputType } = getUnderlyingType(refMutation.type);
  mutationOutputType = getUnderlyingType(Object.values(getTypeFields(mutationOutputType))[1].type).type;
  outputTypeDef.name = prefixTypename(mutationOutputType.name)
  Object.values(getTypeFields(mutationOutputType)).forEach(f => {
    const _f = { name: f.name  };
    const { type: fieldType, wraps } = getUnderlyingType(f.type);
    if (isScalarType(fieldType)) {
      if (inbuiltTypes[fieldType.name]) {
        _f.type = wrapTypename(fieldType.name, wraps)
      } else {
        _f.type = wrapTypename(prefixTypename(fieldType.name), wraps)
        handleType(fieldType, prefixTypename(fieldType.name));
      }
      outputTypeDef.fields.push(_f);
    }
  })
  mutationDefinition.output_type = prefixTypename(mutationOutputType.name);
  newTypes[prefixTypename(outputTypeDef.name)] = outputTypeDef

  return {
    types: Object.values(newTypes),
    action: mutationDefinition
  }

}

module.exports = deriveMutation;
