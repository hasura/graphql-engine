import { getScalarType, getQueryRoot } from '@/features/GraphQLUtils';
import { areTablesEqual } from '@/features/RelationshipsTable';
import { GraphQLType } from 'graphql';
import { GDCTable } from '..';
import {
  exportMetadata,
  runIntrospectionQuery,
  runMetadataQuery,
} from '../../api';
import { GetTableColumnsProps, TableColumn } from '../../types';

/**
 * Refer - https://github.com/hasura/graphql-engine-mono/blob/main/dc-agents/dc-api-types/src/models/TableInfo.ts
 */
type TableInfoResponseType = {
  name: GDCTable;
  columns: { name: string; type: string; nullable: string }[];
  primary_key?: string[] | null;
  description?: string;
  foreign_keys?: Record<
    string,
    {
      foreign_table: GDCTable;
      column_mapping: Record<string, string>;
    }
  >;
};

export const getTableColumns = async (props: GetTableColumnsProps) => {
  const { httpClient, dataSourceName, table } = props;

  try {
    const introspectionResult = await runIntrospectionQuery({ httpClient });
    const { metadata } = await exportMetadata({ httpClient });

    const metadataSource = metadata.sources.find(
      s => s.name === dataSourceName
    );
    const metadataTable = metadataSource?.tables.find(t =>
      areTablesEqual(t.table, table)
    );

    if (!metadataTable) throw Error('No table found in metadata');

    const queryRoot = getQueryRoot({
      defaultQueryRoot: (table as GDCTable).join('_'),
      operation: 'select',
      sourceCustomization: metadataSource?.customization,
      configuration: metadataTable.configuration,
    });

    // eslint-disable-next-line no-underscore-dangle
    const graphQLFields = introspectionResult.data.__schema.types.find(
      (t: any) => t.name === queryRoot
    ).fields;

    const scalarTypes = graphQLFields.map(
      ({ name, type }: { name: string; type: GraphQLType }) => {
        try {
          return { name, type: getScalarType(type) };
        } catch {
          return null;
        }
      }
    );

    const result = await runMetadataQuery<TableInfoResponseType>({
      httpClient,
      body: {
        type: 'get_table_info',
        args: {
          source: dataSourceName,
          table,
        },
      },
    });

    return result.columns.map<TableColumn>(column => {
      const graphqlFieldName =
        metadataTable.configuration?.column_config?.[column.name].custom_name ??
        column.name;
      const scalarType = scalarTypes.find(
        ({ name }: { name: string }) => name === graphqlFieldName
      );
      return {
        name: column.name,
        dataType: column.type,
        graphQLProperties: {
          name: graphqlFieldName,
          scalarType: scalarType.type,
        },
      };
    });
  } catch (error) {
    throw new Error('Error fetching GDC columns');
  }
};
