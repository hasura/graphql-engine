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
