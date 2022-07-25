import { Database, Feature } from '..';
import { runSQL } from '../api';
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

export const bigquery: Database = {
  connectDB: {
    getConfigSchema: async () => {
      return Feature.NotImplemented;
    },
  },
  introspectTables: async (
    dataSourceName: string,
    configuration: BigQueryConfiguration
  ) => {
    const introspectionSql = getIntrospectionSqlQuery(configuration.datasets);

    const tables = await runSQL({
      source: {
        name: dataSourceName,
        kind: 'bigquery',
      },
      sql: introspectionSql,
    });

    return adaptIntrospectedBigQueryTables(tables);
  },
};
