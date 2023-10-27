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
import camelCase from 'lodash/camelCase';

const toPascalCase = (str: string) => {
  return str
    .split('_')
    .map(s => s.charAt(0).toUpperCase() + s.slice(1))
    .join('');
};

const capitalizeFirstLetter = (str: string) => {
  return str.charAt(0).toUpperCase() + str.slice(1);
};

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

const getGraphqlBaseOperationName = (schemaPrefix: string, table: Table) => {
  if (table.configuration?.custom_name) {
    return schemaPrefix
      ? `${schemaPrefix}${capitalizeFirstLetter(
          table.configuration?.custom_name
        )}`
      : table.configuration?.custom_name;
  }
  return schemaPrefix
    ? `${schemaPrefix}${toPascalCase(table?.table?.name)}`
    : `${camelCase(table?.table?.name)}`;
};

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
  if (table.configuration?.custom_name) {
    return '';
  }
  if (source.kind === 'mssql' && schemaName === 'dbo') {
    return '';
  }
  if (
    ['cockroach', 'postgres', 'citus'].includes(source.kind) &&
    schemaName === 'public'
  ) {
    return '';
  }

  return source.customization?.naming_convention === 'graphql-default'
    ? `${table?.table?.schema}`
    : `${table?.table?.schema}_`;
};

export const getOperations = (root: string, microfiber: any) => {
  const queryType = microfiber.getQueryType();
  const mutationType = microfiber.getMutationType();

  // if there are customizations, there is an additional level in the types.
  // using an heuristic to find the correct type
  // if query and mutations have only 1 fields which name contains 'query' and 'mutation' respectively
  // then we assume that the query and mutation types are one level deeper

  let queryTypeName: string = queryType.name;
  let mutationTypeName: string = mutationType.name;

  if (queryType.fields[0].name === 'no_queries_available') {
    return [];
  }

  if (root) {
    queryTypeName = queryType.fields.find((f: any) => f.name === root)?.type
      .name;
  }

  if (root) {
    mutationTypeName = mutationType.fields.find((f: any) => f.name === root)
      ?.type.name;
  }

  const queries = microfiber.getType({
    kind: 'OBJECT',
    name: queryTypeName,
  }).fields;
  const mutations = microfiber.getType({
    kind: 'OBJECT',
    name: mutationTypeName,
  }).fields;

  return [...queries, ...mutations];
};

const generators: Record<EndpointType, Generator> = {
  READ: {
    operationName: (source, table) => {
      if (table?.configuration?.custom_root_fields?.select_by_pk) {
        return table?.configuration?.custom_root_fields?.select_by_pk;
      }
      const schemaPrefix = getSchemaPrefix(source, table);
      const tableName = table.configuration?.custom_name ?? table?.table?.name;
      if (source.customization?.naming_convention === 'graphql-default') {
        const baseOperationName = getGraphqlBaseOperationName(
          schemaPrefix,
          table
        );
        return `${baseOperationName}ByPk`;
      }
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
      if (source.customization?.naming_convention === 'graphql-default') {
        const baseOperationName = getGraphqlBaseOperationName(
          schemaPrefix,
          table
        );
        return baseOperationName;
      }
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
      if (source.customization?.naming_convention === 'graphql-default') {
        const baseOperationName = getGraphqlBaseOperationName(
          schemaPrefix,
          table
        );
        return `insert${capitalizeFirstLetter(baseOperationName)}One`;
      }
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
      if (source.customization?.naming_convention === 'graphql-default') {
        const baseOperationName = getGraphqlBaseOperationName(
          schemaPrefix,
          table
        );
        return `update${capitalizeFirstLetter(baseOperationName)}ByPk`;
      }
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
      if (source.customization?.naming_convention === 'graphql-default') {
        const baseOperationName = getGraphqlBaseOperationName(
          schemaPrefix,
          table
        );
        return `delete${capitalizeFirstLetter(baseOperationName)}ByPk`;
      }
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

      for (const source of metadata?.sources || []) {
        const root = source?.customization?.root_fields?.namespace ?? '';
        const operations = getOperations(root, microfiber);

        const sourcePrefix = source.customization?.root_fields?.prefix || '';

        const sourceSuffix = source.customization?.root_fields?.suffix || '';
        for (const table of source.tables as Table[]) {
          for (const [type, generator] of Object.entries(generators)) {
            let baseOperationName = generator.operationName(source, table);
            if (
              sourcePrefix &&
              source.customization?.naming_convention === 'graphql-default'
            ) {
              baseOperationName =
                baseOperationName[0].toUpperCase() + baseOperationName.slice(1);
            }
            const operationName = `${sourcePrefix}${baseOperationName}${sourceSuffix}`;

            const operation = operations.find(
              operation => operation.name === operationName
            );

            if (!operation) {
              continue;
            }

            const tableName = table?.table?.name;

            const definition = generators[type as EndpointType].generator(
              root,
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
