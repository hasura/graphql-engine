import { isWrappingType, isListType, isNonNullType } from 'graphql';

export const getOperationType = (schema, operation) => {
  if (operation === 'query') {
    return schema._queryType;
  }
  if (operation === 'subscription') {
    return schema._subscriptionType;
  }
  return schema._mutationType;
};

export const getMutationType = schema => {
  return schema._mutationType;
};

export const getUnderlyingType = _type => {
  let type = Object.assign(Object.create(_type), _type);
  const wraps = [];
  while (isWrappingType(type)) {
    if (isListType(type)) wraps.push('l');
    if (isNonNullType(type)) wraps.push('n');
    type = type.ofType;
  }
  return {
    wraps,
    type,
  };
};

export const getTypeFields = _type => {
  return _type._fields || [];
};

export const getFieldArgs = field => {
  return field.args || [];
};

export const getHasuraMutationMetadata = field => {
  if (
    field.args.length === 2 &&
    field.args[0].name === 'objects' &&
    field.args[1].name === 'on_conflict'
  ) {
    return {
      kind: 'insert',
    };
  }

  if (
    field.args.length >= 2 &&
    !!field.args.find(a => a.name === '_set') &&
    !!field.args.find(a => a.name === 'where')
  ) {
    return {
      kind: 'update',
    };
  }

  if (field.args.length === 1 && field.args[0].name === 'where') {
    return {
      kind: 'delete',
    };
  }

  return null;
};
