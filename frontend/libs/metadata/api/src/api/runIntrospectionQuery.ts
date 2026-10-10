import { getIntrospectionQuery, IntrospectionQuery } from 'graphql';
import { NetworkArgs } from '../types';
import { runGraphQL } from './runGraphQL';

export const runIntrospectionQuery = async (args: NetworkArgs) => {
  return runGraphQL<IntrospectionQuery>({
    ...args,
    body: {
      operationName: 'IntrospectionQuery',
      query: getIntrospectionQuery({
        descriptions: true,
        schemaDescription: true,
        typeDepth: 7,
      }),
    },
  });
};
