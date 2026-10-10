import { GraphQLSchema } from 'graphql';
import { Table } from '@hasura/shared/types';
import { useQuery } from '@tanstack/react-query';
import { getAllColumnsAndOperators } from '../utils';
import { useMetadata } from '@hasura/metadata/api';
import { areTablesEqual } from '@hasura/metadata/helpers';

export const useTableConfiguration = ({
  dataSourceName,
  table,
}: {
  dataSourceName: string;
  table: Table;
}) => {
  const { refetch } = useMetadata();
  return useQuery({
    queryKey: ['export_metadata', dataSourceName, table, 'configuration'],
    queryFn: async () => {
      const { data: metadata, error } = await refetch();
      if (!metadata) {
        throw error || new Error('failed to fetch metadata');
      }

      const metadataTable = metadata.metadata.sources
        .find((s) => s.name === dataSourceName)
        ?.tables.find((t) => areTablesEqual(t.table, table));
      if (!metadataTable) throw Error('Unable to find table in metadata');

      return metadataTable.configuration ?? {};
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
