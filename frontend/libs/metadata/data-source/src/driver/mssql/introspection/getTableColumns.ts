import { runSQL } from '@hasura/metadata/api';
import { adaptSQLDataType } from '../utils';
import { GetTableColumnsProps, TableColumn } from '../../types';
import { MssqlTable } from '../types';
import { RunSQLResponse } from '@hasura/shared/types';
import { introspectTableScalarTypes } from '../../common/utils';

const adaptPkResult = (runSQLResult: RunSQLResponse) => {
  return runSQLResult.result?.slice(1).map((row) => row[0]);
};

const adaptTableColumns = (result: RunSQLResponse['result']): TableColumn[] => {
  if (!result) return [];

  return result.slice(1).map((row) => ({
    name: row[0],
    dataType: row[1],
    consoleDataType: adaptSQLDataType(row[1]),
    nullable: !!row[2],
  }));
};

export const getTableColumnInfos = async ({
  dataSourceName,
  table,
  endpoints,
  fetchJson,
}: GetTableColumnsProps) => {
  const { schema, name } = table as MssqlTable;

  const sql = `SELECT COLUMN_NAME, DATA_TYPE, IS_NULLABLE FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = N'${name}' AND TABLE_SCHEMA= N'${schema}'`;

  const tables = await runSQL({
    args: {
      source: {
        name: dataSourceName,
        kind: 'mssql',
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
  const { dataSourceName, table, endpoints, fetchJson } = props;
  const { schema, name } = table as MssqlTable;
  const { scalarTypes, metadataTable } = await introspectTableScalarTypes({
    ...props,
    defaultQueryRoot: schema === 'dbo' ? name : `${schema}_${name}`,
  });

  const primaryKeySql = `SELECT Col.COLUMN_NAME from 
  INFORMATION_SCHEMA.TABLE_CONSTRAINTS Tab, 
  INFORMATION_SCHEMA.CONSTRAINT_COLUMN_USAGE Col 
WHERE 
  Col.Constraint_Name = Tab.Constraint_Name
  AND Col.Table_Name = Tab.Table_Name
  AND Tab.Constraint_Type = 'PRIMARY KEY'
  AND Col.TABLE_NAME = '${name}' AND Col.TABLE_SCHEMA = '${schema}';`;

  const primaryKeysSQLResult = await runSQL({
    args: {
      source: {
        name: dataSourceName,
        kind: 'mssql',
      },
      sql: primaryKeySql,
    },
    fetchJson,
    url: endpoints.queryV2,
  });

  const primaryKeys = adaptPkResult(primaryKeysSQLResult) ?? [];

  const result = sqlResult.map((column) => {
    const graphqlFieldName =
      metadataTable.configuration?.column_config?.[column.name]?.custom_name ??
      column.name;

    const scalarType =
      scalarTypes.find((st) => st?.name === graphqlFieldName) ?? null;

    return {
      name: column.name,
      dataType: column.dataType,
      consoleDataType: column.consoleDataType,
      nullable: column.nullable,
      isPrimaryKey: primaryKeys.includes(column.name),
      graphQLProperties: {
        name: graphqlFieldName,
        scalarType: scalarType?.type,
      },
    };
  });

  return result;
};
