import {
  exportMetadata,
  runIntrospectionQuery,
  runSQL,
  RunSQLResponse,
} from '../../api';
import { BigQueryTable } from '..';
import { GetTableColumnsProps, TableColumn } from '../../types';
import { adaptSQLDataType } from './utils';
import { areTablesEqual } from '../../../hasura-metadata-api';
import { getScalarType, getTypeName } from '../../../GraphQLUtils';
import { GraphQLType } from 'graphql';

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
  const { dataset, name } = table as BigQueryTable;

  const sql = `SELECT column_name, data_type FROM ${dataset}.INFORMATION_SCHEMA.COLUMNS WHERE table_name = '${name}';`;

  const tables = await runSQL({
    source: {
      name: dataSourceName,
      kind: 'bigquery',
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
    defaultQueryRoot: `${dataset}_${name}`,
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
      isPrimaryKey: false,
      graphQLProperties: {
        name: graphqlFieldName,
        scalarType: scalarType?.type ?? null,
      },
    };
  });

  return result;
};
