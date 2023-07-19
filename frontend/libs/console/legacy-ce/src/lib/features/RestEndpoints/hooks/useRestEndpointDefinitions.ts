import { Microfiber } from 'microfiber';
import { useIntrospectionSchema } from '../../../components/Services/Actions/Common/components/ImportTypesModal/useIntrospectionSchema';
import { useEffect, useState } from 'react';
import {
  MetadataTable,
  Query,
  RestEndpoint,
  Source,
} from '../../hasura-metadata-types';
import {
  Operation,
  generateDeleteEndpoint,
  generateInsertEndpoint,
  generateUpdateEndpoint,
  generateViewAllEndpoint,
  generateViewEndpoint,
} from './utils';
import { formatSdl } from 'format-graphql';
import { useMetadata } from '../../hasura-metadata-api';

export type EndpointType = 'READ' | 'READ_ALL' | 'CREATE' | 'UPDATE' | 'DELETE';

export type EndpointDefinition = {
  restEndpoint: RestEndpoint;
  query: Query;
};

type EndpointDefinitions = {
  [key: string]: Partial<
    Record<EndpointType, EndpointDefinition & { exists: boolean }>
  >;
};

type Table = MetadataTable & { table: { name: string; schema: string } };

export type Generator = {
  operationName: (source: Source, table: Table) => string;
  generator: (
    root: string,
    table: string,
    operation: Operation,
    microfiber: any
  ) => EndpointDefinition;
};

export const getSchemaPrefix = (source: Source, table: Table) => {
  const schemaName = table.table.schema;
  if (source.kind === 'mssql' && schemaName === 'dbo') {
    return '';
  }
  if (
    ['cockroach', 'postgres', 'citus'].includes(source.kind) &&
    schemaName === 'public'
  ) {
    return '';
  }

  return `${table?.table?.schema}_`;
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
  READ: {
    operationName: (source, table) => {
      if (table?.configuration?.custom_root_fields?.select_by_pk) {
        return table?.configuration?.custom_root_fields?.select_by_pk;
      }
      const schemaPrefix = getSchemaPrefix(source, table);
      const tableName = table.configuration?.custom_name ?? table?.table?.name;
      return `${schemaPrefix}${tableName}_by_pk`;
    },
    generator: generateViewEndpoint,
  },
  READ_ALL: {
    operationName: (source, table) => {
      if (table?.configuration?.custom_root_fields?.select) {
        return table?.configuration?.custom_root_fields?.select;
      }
      const schemaPrefix = getSchemaPrefix(source, table);
      const tableName = table.configuration?.custom_name ?? table?.table?.name;
      return `${schemaPrefix}${tableName}`;
    },
    generator: generateViewAllEndpoint,
  },
  CREATE: {
    operationName: (source, table) => {
      if (table?.configuration?.custom_root_fields?.insert_one) {
        return table?.configuration?.custom_root_fields?.insert_one;
      }
      const schemaPrefix = getSchemaPrefix(source, table);
      const tableName = table.configuration?.custom_name ?? table?.table?.name;
      return `insert_${schemaPrefix}${tableName}_one`;
    },
    generator: generateInsertEndpoint,
  },
  UPDATE: {
    operationName: (source, table) => {
      if (table?.configuration?.custom_root_fields?.update_by_pk) {
        return table?.configuration?.custom_root_fields?.update_by_pk;
      }
      const schemaPrefix = getSchemaPrefix(source, table);
      const tableName = table.configuration?.custom_name ?? table?.table?.name;
      return `update_${schemaPrefix}${tableName}_by_pk`;
    },
    generator: generateUpdateEndpoint,
  },

  DELETE: {
    operationName: (source, table) => {
      if (table?.configuration?.custom_root_fields?.delete_by_pk) {
        return table?.configuration?.custom_root_fields?.delete_by_pk;
      }
      const schemaPrefix = getSchemaPrefix(source, table);
      const tableName = table.configuration?.custom_name ?? table?.table?.name;
      return `delete_${schemaPrefix}${tableName}_by_pk`;
    },
    generator: generateDeleteEndpoint,
  },
};

export const useRestEndpointDefinitions = () => {
  const {
    data: introspectionSchema,
    isLoading,
    error,
  } = useIntrospectionSchema();

  const { data: metadata } = useMetadata(m => ({
    restEndpoints: m.metadata?.rest_endpoints,
    sources: m.metadata?.sources,
  }));

  const [data, setData] = useState<EndpointDefinitions>();

  useEffect(() => {
    const existingRestEndpoints = metadata?.restEndpoints || [];

    if (introspectionSchema) {
      const response: EndpointDefinitions = {};
      const microfiber = new Microfiber(introspectionSchema);

      const operations = getOperations(microfiber);

      if (!operations) {
        setData({});
        return;
      }

      for (const source of metadata?.sources || []) {
        const sourcePrefix = source.customization?.root_fields?.prefix || '';

        const sourceSuffix = source.customization?.root_fields?.suffix || '';
        for (const table of source.tables as Table[]) {
          for (const [type, generator] of Object.entries(generators)) {
            const operationName = `${sourcePrefix}${generator.operationName(
              source,
              table
            )}${sourceSuffix}`;
            const operation = operations.operations.find(
              operation => operation.name === operationName
            );

            if (!operation) {
              continue;
            }

            const tableName = table?.table?.name;

            const definition = generators[type as EndpointType].generator(
              operations.root,
              tableName,
              operation,
              microfiber
            );

            if (definition.query.query) {
              definition.query.query = formatSdl(definition.query.query);
            }

            response[tableName] = {
              ...(response[tableName] || {}),
              [type]: {
                ...definition,
                exists: existingRestEndpoints.some(
                  endpoint =>
                    endpoint.definition.query.query_name ===
                    definition.query.name
                ),
              },
            };
          }
        }
      }

      setData(response);
    }
  }, [introspectionSchema, metadata]);

  return {
    data,
    isLoading,
    isError: error,
  };
};
