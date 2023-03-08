import { areTablesEqual } from '../../../hasura-metadata-api';
import { GraphQLType } from 'graphql';
import { getScalarType, getTypeName } from '../../../GraphQLUtils';
import { MssqlTable } from '..';
import {
  exportMetadata,
  runIntrospectionQuery,
  runSQL,
  RunSQLResponse,
} from '../../api';
import { adaptSQLDataType } from '../utils';
import { GetTableColumnsProps, TableColumn } from '../../types';

const adaptPkResult = (runSQLResult: RunSQLResponse) => {
  return runSQLResult.result?.slice(1).map(row => row[0]);
};

const adaptTableColumns = (result: RunSQLResponse['result']): TableColumn[] => {
  if (!result) return [];

  return result.slice(1).map(row => ({
    name: row[0],
    dataType: row[1],
    consoleDataType: adaptSQLDataType(row[1]),
    nullable: !!row[2],
  }));
};

export const getTableColumns = async ({
  dataSourceName,
  table,
  httpClient,
}: GetTableColumnsProps) => {
  const { schema, name } = table as MssqlTable;

  const sql = `SELECT COLUMN_NAME, DATA_TYPE, IS_NULLABLE FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = N'${name}' AND TABLE_SCHEMA= N'${schema}'`;

  const tables = await runSQL({
    source: {
      name: dataSourceName,
      kind: 'mssql',
    },
    sql,
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
    defaultQueryRoot: schema === 'dbo' ? name : `${schema}_${name}`,
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

  const primaryKeySql = `SELECT Col.COLUMN_NAME from 
  INFORMATION_SCHEMA.TABLE_CONSTRAINTS Tab, 
  INFORMATION_SCHEMA.CONSTRAINT_COLUMN_USAGE Col 
WHERE 
  Col.Constraint_Name = Tab.Constraint_Name
  AND Col.Table_Name = Tab.Table_Name
  AND Tab.Constraint_Type = 'PRIMARY KEY'
  AND Col.TABLE_NAME = '${name}' AND Col.TABLE_SCHEMA = '${schema}';`;

  const primaryKeysSQLResult = await runSQL({
    source: {
      name: dataSourceName,
      kind: 'mssql',
    },
    sql: primaryKeySql,
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
