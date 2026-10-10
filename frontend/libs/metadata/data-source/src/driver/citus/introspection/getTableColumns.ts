import { CitusTable } from '../types';
import { runSQL } from '@hasura/metadata/api';
import { GetTableColumnsProps, TableColumn } from '../../types';
import { adaptSQLDataType, adaptStringForPostgres } from '../../postgres/utils';
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
    nullable: row[2] === 'YES',
    defaultValue: row[3] ?? null,
  }));
};

export const getTableColumnInfos = async ({
  dataSourceName,
  table,
  endpoints,
  fetchJson,
}: GetTableColumnsProps) => {
  const { schema, name } = table as CitusTable;

  const sql = `
  SELECT 
   column_name, data_type, is_nullable, column_default
  FROM 
    information_schema.columns 
  WHERE 
    table_schema = '${schema}' AND 
    table_name  = '${name}';`;

  const tables = await runSQL({
    args: {
      source: {
        name: dataSourceName,
        kind: 'citus',
      },
      sql,
      readOnly: true,
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
  const { dataSourceName, table, fetchJson, endpoints } = props;
  const { schema, name } = table as CitusTable;

  const { scalarTypes, metadataTable } = await introspectTableScalarTypes({
    ...props,
    defaultQueryRoot: schema === 'public' ? name : `${schema}_${name}`,
  });

  const primaryKeySql = `SELECT a.attname
  FROM   pg_index i
  JOIN   pg_attribute a ON a.attrelid = i.indrelid
                       AND a.attnum = ANY(i.indkey)
  WHERE  i.indrelid = '${adaptStringForPostgres(
    schema,
  )}.${adaptStringForPostgres(name)}'::regclass
  AND    i.indisprimary;`;

  const primaryKeysSQLResult = await runSQL({
    args: {
      source: {
        name: dataSourceName,
        kind: 'citus',
      },
      sql: primaryKeySql,
      readOnly: true,
    },
    fetchJson,
    url: endpoints.queryV2,
  });

  const primaryKeys = adaptPkResult(primaryKeysSQLResult) ?? [];

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
      defaultValue: column.defaultValue,
      isPrimaryKey: primaryKeys.includes(column.name),
      graphQLProperties: {
        name: graphqlFieldName,
        scalarType: scalarType?.type,
      },
    };
  });

  return result;
};
