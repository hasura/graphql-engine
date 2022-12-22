/* eslint-disable no-underscore-dangle */
import { getScalarType, getTypeName } from '@/features/GraphQLUtils';
import { areTablesEqual } from '@/features/RelationshipsTable';
import { GraphQLType } from 'graphql';
import { GDCTable } from '..';
import {
  exportMetadata,
  runIntrospectionQuery,
  runMetadataQuery,
} from '../../api';
import { GetTableColumnsProps, TableColumn } from '../../types';
import { GetTableInfoResponse } from './types';

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

    const queryRoot = getTypeName({
      defaultQueryRoot: (table as GDCTable).join('_'),
      operation: 'select',
      sourceCustomization: metadataSource?.customization,
      configuration: metadataTable.configuration,
    });
    // eslint-disable-next-line no-underscore-dangle
    const graphQLFields =
      introspectionResult.data.__schema.types.find(
        (t: any) => t.name === queryRoot
      )?.fields ?? [];

    const scalarTypes = graphQLFields
      .map(({ name, type }: { name: string; type: GraphQLType }) => {
        try {
          return { name, type: getScalarType(type) };
        } catch {
          return null;
        }
      })
      .filter(Boolean);

    const tableInfo = await runMetadataQuery<GetTableInfoResponse>({
      httpClient,
      body: {
        type: 'get_table_info',
        args: {
          source: dataSourceName,
          table,
        },
      },
    });

    const primaryKeys = tableInfo?.primary_key ? tableInfo.primary_key : [];

    return tableInfo.columns.map<TableColumn>(column => {
      const graphqlFieldName =
        metadataTable.configuration?.column_config?.[column.name]
          ?.custom_name ?? column.name;

      const scalarType =
        scalarTypes.find(
          ({ name }: { name: string }) => name === graphqlFieldName
        ) ?? null;

      return {
        name: column.name,
        dataType: column.type,
        nullable: column.nullable,
        isPrimaryKey: primaryKeys.includes(column.name),
        graphQLProperties: {
          name: graphqlFieldName,
          scalarType: scalarType?.type ?? null,
        },
      };
    });
  } catch (error) {
    console.error(error);
    throw new Error('Error fetching GDC columns');
  }
};
