import {
  GraphQLInputObjectType,
  GraphQLInputFieldMap,
  GraphQLSchema,
} from 'graphql';

export const getOperationType = (schema: GraphQLSchema, operation: string) => {
  if (operation === 'query') {
    return schema.getQueryType();
  }
  if (operation === 'subscription') {
    return schema.getSubscriptionType();
  }
  return schema.getMutationType();
};

export const getTypeFields = (
  _type: GraphQLInputObjectType,
): GraphQLInputFieldMap => {
  return _type.getFields() || {};
};

export const getFieldArgs = (field) => {
  return field.args || [];
};

export const getHasuraMutationMetadata = (field) => {
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
    !!field.args.find((a) => a.name === '_set') &&
    !!field.args.find((a) => a.name === 'where')
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
