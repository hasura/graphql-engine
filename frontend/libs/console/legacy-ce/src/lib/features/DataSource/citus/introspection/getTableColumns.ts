/* eslint-disable no-underscore-dangle */
/* eslint-disable @typescript-eslint/no-unused-vars */
import { getScalarType, getTypeName } from '../../../GraphQLUtils';
import { areTablesEqual } from '../../../hasura-metadata-api';
import { GraphQLType } from 'graphql';
import { CitusTable } from '..';
import {
  exportMetadata,
  runIntrospectionQuery,
  runSQL,
  RunSQLResponse,
} from '../../api';
import { GetTableColumnsProps, TableColumn } from '../../types';
import { adaptSQLDataType, adaptStringForPostgres } from '../../postgres/utils';

const adaptPkResult = (runSQLResult: RunSQLResponse) => {
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
  const { schema, name } = table as CitusTable;

  const sql = `
  SELECT 
   column_name, data_type, is_nullable
  FROM 
    information_schema.columns 
  WHERE 
    table_schema = '${schema}' AND 
    table_name  = '${name}';`;

  const tables = await runSQL({
    source: {
      name: dataSourceName,
      kind: 'citus',
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
    };
  });

  return result;
};
