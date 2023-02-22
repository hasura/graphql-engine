import React from 'react';
import { buildClientSchema, GraphQLSchema, IntrospectionQuery } from 'graphql';
import { useHttpClient } from '../../../../../Network';
import {
  exportMetadata,
  runIntrospectionQuery,
} from '../../../../../DataSource';
import { Table } from '../../../../../hasura-metadata-types';
import { useQuery } from 'react-query';
import { getAllColumnsAndOperators } from '../utils';
import { areTablesEqual } from '../../../../../hasura-metadata-api';

/**
 *
 * fetch the schema from the gql server
 */
export const useIntrospectSchema = () => {
  const httpClient = useHttpClient();

  const [schema, setSchema] = React.useState<GraphQLSchema>();

  const getSchema = React.useCallback(async () => {
    const introspectionResult: { data: IntrospectionQuery } =
      await runIntrospectionQuery({ httpClient });
    const result = buildClientSchema(introspectionResult.data);
    setSchema(result);
  }, []);

  React.useEffect(() => {
    getSchema();
  }, [getSchema]);

  return { data: schema };
};

export const useTableConfiguration = ({
  dataSourceName,
  table,
}: {
  dataSourceName: string;
  table: Table;
}) => {
  const httpClient = useHttpClient();
  return useQuery({
    queryKey: ['export_metadata', dataSourceName, table, 'configuration'],
    queryFn: async () => {
      const { metadata } = await exportMetadata({ httpClient });
      const metadataTable = metadata.sources
        .find(s => s.name === dataSourceName)
        ?.tables.find(t => areTablesEqual(t.table, table));
      if (!metadata) throw Error('Unable to find table in metadata');

      return metadataTable?.configuration ?? {};
    },
  });
};

interface Args {
  tableName: string;
  schema?: GraphQLSchema;
  table: Table;
  dataSourceName: string;
}

/**
 *
 * get all boolOperators, columns and relationships
 * and information about types for each
 */
export const useData = ({ tableName, schema, table, dataSourceName }: Args) => {
  const { data: tableConfig } = useTableConfiguration({
    table,
    dataSourceName,
  });
  if (!schema)
    return {
      data: {
        boolOperators: [],
        existOperators: [],
        columns: [],
        relationships: [],
      },
    };

  const data = getAllColumnsAndOperators({ tableName, schema, tableConfig });
  return { data, tableConfig };
};
