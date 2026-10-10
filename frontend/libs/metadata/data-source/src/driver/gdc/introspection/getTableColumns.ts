import { isObjectType } from 'graphql';
import { GDCTable } from '@hasura/shared/types';
import { GetTableColumnsProps, TableColumn } from '../../types';
import { adaptAgentDataType } from './utils';
import { GetTableInfoResponse } from './types';
import { runMetadataQuery } from '@hasura/metadata/api';
import { getTableDisplayName } from '@hasura/shared/utils';
import { introspectTableScalarTypes } from '../../common/utils';

export const getTableInfo = async (props: GetTableColumnsProps) => {
  const { fetchJson, dataSourceName, table, endpoints } = props;
  return runMetadataQuery<GetTableInfoResponse>({
    url: endpoints.metadata,
    fetchJson,
    body: {
      type: 'get_table_info',
      args: {
        source: dataSourceName,
        table,
      },
    },
  });
};

export const getTableColumnInfos = async (
  props: GetTableColumnsProps,
): Promise<TableColumn[]> => {
  const tableInfo = await getTableInfo(props);

  return tableInfo.columns.map((column) => {
    return {
      name: column.name,
      dataType: adaptAgentDataType(column.type),
      /**
        Will be updated once GDC supports mutations
      */
      consoleDataType: 'string',
      nullable: column.nullable,
      value_generated: column.value_generated,
    };
  });
};

export const getTableColumns = async (
  props: GetTableColumnsProps,
): Promise<TableColumn[]> => {
  try {
    const tableInfo = await getTableInfo(props);

    const { scalarTypes, schema, metadataTable } =
      await introspectTableScalarTypes({
        ...props,
        defaultQueryRoot: (props.table as GDCTable).join('_'),
      });

    const primaryKeys = tableInfo?.primary_key ? tableInfo.primary_key : [];
    const tableName = getTableDisplayName(tableInfo.name);
    const type = schema.getType(tableName);
    const fields = isObjectType(type) ? type.getFields() : {};

    return tableInfo.columns
      .map((column) => {
        const graphqlFieldName =
          metadataTable.configuration?.column_config?.[column.name]
            ?.custom_name ?? column.name;

        const scalarType = scalarTypes.find(
          (st) => st?.name === graphqlFieldName,
        );

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
            scalarType: scalarType?.type,
            graphQLType: field?.type,
          },
          value_generated: column.value_generated,
        };
      })
      .filter(Boolean) as TableColumn[];
  } catch (error) {
    console.error(error);
    throw new Error('Error fetching GDC columns');
  }
};
