import { isObjectType } from 'graphql';
import { GDCTable } from '@hasura/shared/types';
import { GetTableColumnsProps, TableColumn } from '../../types';
import {
  adaptAgentDataType,
  getScalarConsoleDataType,
  ScalarTypeCapabilities,
} from './utils';
import { GetTableInfoResponse } from './types';
import { exportMetadata, runMetadataQuery } from '@hasura/metadata/api';
import { getTableDisplayName } from '@hasura/shared/utils';
import { introspectTableScalarTypes } from '../../common/utils';
import { getDriverCapabilities } from './getDriverCapabilities';

/**
 * Returns the capabilities of the scalar types of the source's agent, which
 * tell the GraphQL type of each column type.
 */
const getScalarTypesCapabilities = async (
  { dataSourceName, endpoints, fetchJson }: GetTableColumnsProps,
  sourceKind?: string,
): Promise<Record<string, ScalarTypeCapabilities>> => {
  const driver =
    sourceKind ??
    (
      await exportMetadata({ fetchJson, url: endpoints.metadata })
    ).metadata.sources.find(({ name }) => name === dataSourceName)?.kind;
  if (!driver) return {};

  const capabilities = await getDriverCapabilities({
    driver,
    endpoints,
    fetchJson,
  });
  return capabilities?.scalar_types ?? {};
};

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
  const [tableInfo, scalarTypesCapabilities] = await Promise.all([
    getTableInfo(props),
    getScalarTypesCapabilities(props),
  ]);

  return tableInfo.columns.map((column) => {
    return {
      name: column.name,
      dataType: adaptAgentDataType(column.type),
      consoleDataType: getScalarConsoleDataType(
        column.type,
        scalarTypesCapabilities[column.type],
      ),
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

    const { scalarTypes, schema, metadataSource, metadataTable } =
      await introspectTableScalarTypes({
        ...props,
        defaultQueryRoot: (props.table as GDCTable).join('_'),
      });
    const scalarTypesCapabilities = await getScalarTypesCapabilities(
      props,
      metadataSource?.kind,
    );

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
          consoleDataType: getScalarConsoleDataType(
            column.type,
            scalarTypesCapabilities[column.type],
          ),
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
    throw Object.assign(new Error('Error fetching GDC columns'), {
      cause: error,
    });
  }
};
