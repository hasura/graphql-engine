import { runSQL } from '@hasura/metadata/api';
import { GetTrackableTablesProps } from '../../types';
import { adaptIntrospectedBigQueryTables } from './utils';

type BigQueryConfiguration = {
  datasets: string[];
};

const getDatasetIntrospectQuery = (dataset: string) => `
select
table_name,
table_schema,
table_type, 
FROM ${dataset}.INFORMATION_SCHEMA.TABLES
`;

const getIntrospectionSqlQuery = (
  datasets: BigQueryConfiguration['datasets'],
) =>
  datasets
    .map((dataset) => getDatasetIntrospectQuery(dataset))
    .join('union all');

export const getTrackableTables = async ({
  dataSourceName,
  configuration,
  endpoints,
  fetchJson,
}: GetTrackableTablesProps) => {
  const introspectionSql = getIntrospectionSqlQuery(configuration.datasets);

  const tables = await runSQL({
    args: {
      source: {
        name: dataSourceName,
        kind: 'bigquery',
      },
      sql: introspectionSql,
    },
    fetchJson,
    url: endpoints.queryV2,
  });

  return adaptIntrospectedBigQueryTables(tables);
};
