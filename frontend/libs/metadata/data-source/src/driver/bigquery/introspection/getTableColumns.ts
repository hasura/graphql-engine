import { BigQueryTable } from '../types';
import { GetTableColumnsProps, TableColumn } from '../../types';
import { adaptSQLDataType } from './utils';
import { runSQL } from '@hasura/metadata/api';
import { RunSQLResponse } from '@hasura/shared/types';
import { introspectTableScalarTypes } from '../../common/utils';

const adaptTableColumns = (result: RunSQLResponse['result']): TableColumn[] => {
  if (!result) return [];

  return result.slice(1).map((row) => ({
    name: row[0],
    dataType: row[1],
    consoleDataType: adaptSQLDataType(row[1]),
    nullable: row[2] === 'YES',
  }));
};

export const getTableColumnInfos = async ({
  dataSourceName,
  table,
  endpoints,
  fetchJson,
}: GetTableColumnsProps) => {
  const { dataset, name } = table as BigQueryTable;

  const sql = `SELECT column_name, data_type FROM ${dataset}.INFORMATION_SCHEMA.COLUMNS WHERE table_name = '${name}';`;

  const tables = await runSQL({
    args: {
      source: {
        name: dataSourceName,
        kind: 'bigquery',
      },
      sql,
    },
    fetchJson,
    url: endpoints.queryV2,
  });

  return adaptTableColumns(tables.result);
};

export const getTableColumns = async (
  props: GetTableColumnsProps,
): Promise<TableColumn[]> => {
  const sqlResult = await getTableColumnInfos(props);
  const { dataset, name } = props.table as BigQueryTable;
  const { scalarTypes, metadataTable } = await introspectTableScalarTypes({
    ...props,
    defaultQueryRoot: `${dataset}_${name}`,
  });

  const result = sqlResult.map((column) => {
    const graphqlFieldName =
      metadataTable.configuration?.column_config?.[column.name]?.custom_name ??
      column.name;

    const scalarType = scalarTypes.find((st) => st?.name === graphqlFieldName);

    return {
      name: column.name,
      dataType: column.dataType,
      consoleDataType: column.consoleDataType,
      nullable: column.nullable,
      isPrimaryKey: false,
      graphQLProperties: {
        name: graphqlFieldName,
        scalarType: scalarType?.type,
      },
    };
  });

  return result;
};
