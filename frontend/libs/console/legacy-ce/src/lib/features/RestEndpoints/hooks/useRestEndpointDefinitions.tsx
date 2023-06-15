import { Microfiber } from 'microfiber';
import { useIntrospectionSchema } from '../../../components/Services/Actions/Common/components/ImportTypesModal/useIntrospectionSchema';
import { useEffect, useState } from 'react';
import { Query, RestEndpoint } from '../../hasura-metadata-types';
import {
  generateDeleteEndpoint,
  generateInsertEndpoint,
  generateUpdateEndpoint,
  generateViewAllEndpoint,
  generateViewEndpoint,
} from './utils';
import { formatSdl } from 'format-graphql';

export type EndpointType = 'VIEW' | 'VIEW_ALL' | 'CREATE' | 'UPDATE' | 'DELETE';

export type EndpointDefinition = {
  restEndpoint: RestEndpoint;
  query: Query;
};

type EndpointDefinitions = {
  [key: string]: Partial<Record<EndpointType, EndpointDefinition>>;
};

export type Generator = {
  regExp: RegExp;
  generator: (
    root: string,
    table: string,
    operation: any,
    microfiber: any
  ) => EndpointDefinition;
};

export const getOperations = (microfiber: any) => {
  const queryType = microfiber.getQueryType();
  const mutationType = microfiber.getMutationType();

  // if there are customizations, there is an additional level in the types.
  // using an heuristic to find the correct type
  // if query and mutations have only 1 fields which name contains 'query' and 'mutation' respectively
  // then we assume that the query and mutation types are one level deeper

  let queryTypeName: string = queryType.name;
  let mutationTypeName: string = mutationType.name;

  let root = '';

  if (queryType.fields[0].name === 'no_queries_available') {
    return {
      root: '',
      operations: [],
    };
  }

  if (
    queryType.fields.length === 1 &&
    queryType.fields[0].type.name.includes('query')
  ) {
    queryTypeName = queryType.fields[0].type.name;
    root = queryType.fields[0].name;
  }

  if (
    mutationType.fields.length === 1 &&
    mutationType.fields[0].type.name.includes('mutation')
  ) {
    mutationTypeName = mutationType.fields[0].type.name;
    root = mutationType.fields[0].name;
  }

  const queries = microfiber.getType({
    kind: 'OBJECT',
    name: queryTypeName,
  }).fields;
  const mutations = microfiber.getType({
    kind: 'OBJECT',
    name: mutationTypeName,
  }).fields;

  return {
    root,
    operations: [...queries, ...mutations],
  };
};

const generators: Record<EndpointType, Generator> = {
  VIEW: {
    regExp: /fetch data from the table: "(.+)" using primary key columns$/,
    generator: generateViewEndpoint,
  },
  VIEW_ALL: {
    regExp: /fetch data from the table: "(.+)"$/,
    generator: generateViewAllEndpoint,
  },
  CREATE: {
    regExp: /insert a single row into the table: "(.+)"$/,
    generator: generateInsertEndpoint,
  },
  UPDATE: {
    regExp: /update single row of the table: "(.+)"$/,
    generator: generateUpdateEndpoint,
  },

  DELETE: {
    regExp: /delete single row from the table: "(.+)"$/,
    generator: generateDeleteEndpoint,
  },
};

export const useRestEndpointDefinitions = () => {
  const {
    data: introspectionSchema,
    isLoading,
    error,
  } = useIntrospectionSchema();

  const [data, setData] = useState<EndpointDefinitions>();

  useEffect(() => {
    if (introspectionSchema) {
      const response: EndpointDefinitions = {};
      const microfiber = new Microfiber(introspectionSchema);

      const operations = getOperations(microfiber);

      if (!operations) {
        setData({});
        return;
      }

      for (const operation of operations.operations) {
        for (const endpointType in generators) {
          const match = operation.description.match(
            generators[endpointType as EndpointType].regExp
          );
          const table = match?.[1];

          if (match) {
            const definition = generators[
              endpointType as EndpointType
            ].generator(operations.root, table, operation, microfiber);

            if (definition.query.query) {
              definition.query.query = formatSdl(definition.query.query);
            }

            response[table] = {
              ...(response[table] || {}),
              [endpointType]: definition,
            };
          }
        }
      }
      setData(response);
    }
  }, [introspectionSchema]);

  return {
    data,
    isLoading,
    isError: error,
  };
};
