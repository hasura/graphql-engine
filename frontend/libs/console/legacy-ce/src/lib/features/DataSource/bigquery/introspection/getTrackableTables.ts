import { runSQL } from '../../api';
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
  datasets: BigQueryConfiguration['datasets']
) =>
  datasets.map(dataset => getDatasetIntrospectQuery(dataset)).join('union all');

export const getTrackableTables = async ({
  dataSourceName,
  configuration,
  httpClient,
}: GetTrackableTablesProps) => {
  const introspectionSql = getIntrospectionSqlQuery(configuration.datasets);

  const tables = await runSQL({
    source: {
      name: dataSourceName,
      kind: 'bigquery',
    },
    sql: introspectionSql,
    httpClient,
  });

  return adaptIntrospectedBigQueryTables(tables);
};
