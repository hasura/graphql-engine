import { getScalarType, getTypeName } from '../../../GraphQLUtils';
import { areTablesEqual } from '../../../hasura-metadata-api';
import { GraphQLType } from 'graphql';
import { PostgresTable } from '..';
import {
  exportMetadata,
  runIntrospectionQuery,
  runSQL,
  RunSQLResponse,
} from '../../api';
import { GetTableColumnsProps, TableColumn } from '../../types';
import { adaptSQLDataType, adaptStringForPostgres } from '../utils';

const adaptPkResult = (runSQLResult: RunSQLResponse) => {
  return runSQLResult.result?.slice(1).map(row => row[0]);
};

const adaptSequencesResult = (
  runSQLResult: RunSQLResponse,
  table: PostgresTable
) => {
  console.log(runSQLResult.result?.slice(1));

  return runSQLResult.result?.slice(1).map(row => row[0]);
};

const adaptTableColumns = (result: RunSQLResponse['result']): TableColumn[] => {
  if (!result) return [];

  return result.slice(1).map(row => ({
    name: row[0],
    dataType: row[1],
    consoleDataType: adaptSQLDataType(row[1]),
    nullable: row[2] === 'YES',
  }));
};

export const getTableColumns = async ({
  dataSourceName,
  table,
  httpClient,
}: GetTableColumnsProps) => {
  const { schema, name } = table as PostgresTable;

  const sql = `
  SELECT a.attname as column_name,
       pg_catalog.format_type(a.atttypid, NULL) as data_type,
       case
        when a.attnotnull = 'f' then 'YES'
        else 'NO'
       end as is_nullable
FROM pg_attribute a
  JOIN pg_class t on a.attrelid = t.oid
  JOIN pg_namespace s on t.relnamespace = s.oid
WHERE a.attnum > 0 
  AND NOT a.attisdropped
  AND t.relname = '${name}'
  AND s.nspname = '${schema}' 
`;

  const tables = await runSQL({
    source: {
      name: dataSourceName,
      kind: 'postgres',
    },
    sql,
    readOnly: true,
    httpClient,
  });

  const sqlResult = adaptTableColumns(tables.result);

  const introspectionResult = await runIntrospectionQuery({ httpClient });

  const { metadata } = await exportMetadata({ httpClient });

  if (!metadata) throw Error('Metadata could not be retrieved');

  const metadataSource = metadata.sources.find(s => s.name === dataSourceName);

  const metadataTable = metadataSource?.tables.find(t =>
    areTablesEqual(t.table, table)
  );

  if (!metadataTable) throw Error('No table found in metadata');

  const queryRoot = getTypeName({
    defaultQueryRoot: schema === 'public' ? name : `${schema}_${name}`,
    operation: 'select',
    sourceCustomization: metadataSource?.customization,
    configuration: metadataTable.configuration,
  });

  const graphQLFields =
    // eslint-disable-next-line no-underscore-dangle
    introspectionResult.data.__schema.types.find(
      (t: any) => t.name === queryRoot
    )?.fields ?? [];

  const scalarTypes = graphQLFields
    .map(({ name: _name, type }: { name: string; type: GraphQLType }) => {
      try {
        return { name: _name, type: getScalarType(type) };
      } catch {
        return null;
      }
    })
    .filter(Boolean);

  const primaryKeySql = `SELECT a.attname
  FROM   pg_index i
  JOIN   pg_attribute a ON a.attrelid = i.indrelid
                       AND a.attnum = ANY(i.indkey)
  WHERE  i.indrelid = '${adaptStringForPostgres(
    schema
  )}.${adaptStringForPostgres(name)}'::regclass
  AND    i.indisprimary;`;

  const primaryKeysSQLResult = await runSQL({
    source: {
      name: dataSourceName,
      kind: 'postgres',
    },
    sql: primaryKeySql,
    readOnly: true,
    httpClient,
  });

  const primaryKeys = adaptPkResult(primaryKeysSQLResult) ?? [];

  const sequencesSQL = `
  SELECT column_name FROM information_schema.columns WHERE column_default LIKE 'nextval%' and table_schema = '${schema}' and table_name = '${name}';
  `;

  const sequencesSQLResult = await runSQL({
    source: {
      name: dataSourceName,
      kind: 'postgres',
    },
    sql: sequencesSQL,
    readOnly: true,
    httpClient,
  });

  const serialColumns =
    adaptSequencesResult(sequencesSQLResult, { schema, name }) ?? [];

  console.log(serialColumns);

  const result = sqlResult.map<TableColumn>(column => {
    const graphqlFieldName =
      metadataTable.configuration?.column_config?.[column.name]?.custom_name ??
      column.name;

    const scalarType =
      scalarTypes.find(
        ({ name: _name }: { name: string }) => _name === graphqlFieldName
      ) ?? null;

    return {
      name: column.name,
      dataType: column.dataType,
      consoleDataType: column.consoleDataType,
      nullable: column.nullable,
      isPrimaryKey: primaryKeys.includes(column.name),
      graphQLProperties: {
        name: graphqlFieldName,
        scalarType: scalarType?.type ?? null,
      },
      ...(serialColumns.includes(column.name) && {
        value_generated: {
          type: 'auto_increment',
        },
      }),
    };
  });

  return result;
};
