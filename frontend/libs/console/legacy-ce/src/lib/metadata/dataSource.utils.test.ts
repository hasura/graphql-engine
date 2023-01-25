import { DataSourceDriver, getDataSourcePrefix } from './dataSource.utils';

describe('getDataSourcePrefix', () => {
  it.each`
    driver         | expected
    ${'mysql'}     | ${'mysql_'}
    ${'mssql'}     | ${'mssql_'}
    ${'bigquery'}  | ${'bigquery_'}
    ${'citus'}     | ${'citus_'}
    ${'cockroach'} | ${'cockroach_'}
    ${'alloy'}     | ${'alloy_'}
    ${'postgres'}  | ${'pg_'}
  `('returns prefix $expected for driver $driver', ({ driver, expected }) => {
    expect(getDataSourcePrefix(driver as DataSourceDriver)).toEqual(expected);
  });
});
