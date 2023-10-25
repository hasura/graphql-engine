/* eslint-disable no-underscore-dangle */
import { getScalarType, getTypeName } from '../../../GraphQLUtils';
import { GraphQLType, buildClientSchema, isObjectType } from 'graphql';
import { GDCTable } from '..';
import {
  exportMetadata,
  runIntrospectionQuery,
  runMetadataQuery,
} from '../../api';
import { GetTableColumnsProps, TableColumn } from '../../types';
import { adaptAgentDataType } from './utils';
import { GetTableInfoResponse } from './types';
import { areTablesEqual } from '../../../hasura-metadata-api';
import { getTableDisplayName } from '../../../DatabaseRelationships';

export const getTableColumns = async (props: GetTableColumnsProps) => {
  const { httpClient, dataSourceName, table } = props;

  try {
    const introspectionResult = await runIntrospectionQuery({ httpClient });
    const { metadata } = await exportMetadata({ httpClient });

    if (!metadata) throw Error('Metadata could not be retrieved');

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
    const schema = buildClientSchema(introspectionResult.data);

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
    const tableName = getTableDisplayName(tableInfo.name);
    const type = schema.getType(tableName);
    const fields = isObjectType(type) ? type.getFields() : {};

    return tableInfo.columns.map<TableColumn>(column => {
      const graphqlFieldName =
        metadataTable.configuration?.column_config?.[column.name]
          ?.custom_name ?? column.name;

      const scalarType =
        scalarTypes.find(
          ({ name }: { name: string }) => name === graphqlFieldName
        ) ?? null;
      const field = fields[graphqlFieldName];
      return {
        name: column.name,
        dataType: adaptAgentDataType(column.type),
        /**
          Will be updated once GDC supports mutations
        */
        consoleDataType: 'string',
        nullable: column.nullable,
        isPrimaryKey: primaryKeys.includes(column.name),
        graphQLProperties: {
          name: graphqlFieldName,
          scalarType: scalarType?.type ?? null,
          graphQLType: field?.type,
        },
        value_generated: column.value_generated,
      };
    });
  } catch (error) {
    console.error(error);
    throw new Error('Error fetching GDC columns');
  }
};
