import { PostgresTable } from '../types';
import { runSQL } from '@hasura/metadata/api';
import { GetTableColumnsProps, TableColumn } from '../../types';
import { adaptSQLDataType, adaptStringForPostgres } from '../utils';
import { RunSQLResponse } from '@hasura/shared/types';
import { introspectTableScalarTypes } from '../../common/utils';

const adaptPkResult = (runSQLResult: RunSQLResponse) => {
  return runSQLResult.result?.slice(1).map((row) => row[0]);
};

const adaptSequencesResult = (runSQLResult: RunSQLResponse) => {
  return runSQLResult.result?.slice(1).map((row) => row[0]);
};

const adaptTableColumns = (result: RunSQLResponse['result']): TableColumn[] => {
  if (!result) return [];

  return result.slice(1).map((row) => ({
    name: row[0],
    dataType: row[1],
    consoleDataType: adaptSQLDataType(row[1]),
    nullable: row[2] === 'YES',
    sqlType: row[3],
    defaultValue: row[4] ?? null,
  }));
};

export const getTableColumnInfos = async ({
  dataSourceName,
  table,
  endpoints,
  fetchJson,
}: GetTableColumnsProps) => {
  const { schema, name } = table as PostgresTable;

  const sql = `
  SELECT a.attname as column_name,
       pg_catalog.format_type(a.atttypid, NULL) as data_type,
       case
        when a.attnotnull = 'f' then 'YES'
        else 'NO'
       end as is_nullable,
       pg_catalog.format_type(a.atttypid, a.atttypmod) as sql_type,
       pg_catalog.pg_get_expr(d.adbin, d.adrelid) as column_default
FROM pg_attribute a
  JOIN pg_class t on a.attrelid = t.oid
  JOIN pg_namespace s on t.relnamespace = s.oid
  LEFT JOIN pg_attrdef d on d.adrelid = a.attrelid AND d.adnum = a.attnum
WHERE a.attnum > 0 
  AND NOT a.attisdropped
  AND t.relname = '${name}'
  AND s.nspname = '${schema}' 
`;

  const tables = await runSQL({
    args: {
      source: {
        name: dataSourceName,
        kind: 'postgres',
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
  const { dataSourceName, table, endpoints, fetchJson } = props;
  const { schema, name } = table as PostgresTable;

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
        kind: 'postgres',
      },
      sql: primaryKeySql,
      readOnly: true,
    },
    fetchJson,
    url: endpoints.queryV2,
  });

  const primaryKeys = adaptPkResult(primaryKeysSQLResult) ?? [];

  const sequencesSQL = `
  SELECT column_name FROM information_schema.columns WHERE column_default LIKE 'nextval%' and table_schema = '${schema}' and table_name = '${name}';
  `;

  const sequencesSQLResult = await runSQL({
    args: {
      source: {
        name: dataSourceName,
        kind: 'postgres',
      },
      sql: sequencesSQL,
      readOnly: true,
    },
    fetchJson,
    url: endpoints.queryV2,
  });

  const serialColumns = adaptSequencesResult(sequencesSQLResult) ?? [];

  const result = sqlResult
    .map((column) => {
      const graphqlFieldName =
        metadataTable.configuration?.column_config?.[column.name]
          ?.custom_name ?? column.name;

      const scalarType = scalarTypes.find(
        (st) => st?.name === graphqlFieldName,
      );

      return {
        name: column.name,
        dataType: column.dataType,
        consoleDataType: column.consoleDataType,
        nullable: column.nullable,
        sqlType: column.sqlType,
        defaultValue: column.defaultValue,
        isPrimaryKey: primaryKeys.includes(column.name),
        graphQLProperties: {
          name: graphqlFieldName,
          scalarType: scalarType?.type,
        },
        ...(serialColumns.includes(column.name) && {
          value_generated: {
            type: 'auto_increment',
          },
        }),
      };
    })
    .filter(Boolean) as TableColumn[];

  return result;
};
