import { checkDefaultGQLScalarType } from '@/components/Services/RemoteSchema/Permissions/utils';
import { GraphQLSchema } from 'graphql';

export const getTypesFromIntrospection = (data: GraphQLSchema) => {
  return Object.entries(data.getTypeMap())
    .map(([typeName, x]) => ({
      typeName,
      // eslint-disable-next-line no-underscore-dangle
      fields: Object.keys((x as any)._fields || {}),
    }))
    .filter(
      ({ typeName }) =>
        !checkDefaultGQLScalarType(typeName) && !typeName.startsWith('__')
    );
};
