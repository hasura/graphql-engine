import axios from 'axios';
import { DataSource } from '..';

describe('Verify capabilities for native drivers', () => {
  it('[POSTGRES]', async () => {
    const httpClient = axios.create();
    const result = await DataSource(httpClient).getDriverCapabilities(
      'postgres'
    );
    expect(result).toMatchInlineSnapshot(`
      {
        "data_schema": {
          "supports_foreign_keys": true,
        },
        "interpolated_queries": {},
        "mutations": {
          "delete": {},
          "insert": {},
          "update": {},
        },
        "queries": {
          "foreach": {},
        },
        "relationships": {},
        "user_defined_functions": {},
      }
    `);
  });
  it('[COCKROACH]', async () => {
    const httpClient = axios.create();
    const result = await DataSource(httpClient).getDriverCapabilities(
      'cockroach'
    );
    expect(result).toMatchInlineSnapshot(`
      {
        "data_schema": {
          "supports_foreign_keys": true,
        },
        "interpolated_queries": {},
        "mutations": {
          "delete": {},
          "insert": {},
          "update": {},
        },
        "queries": {
          "foreach": {},
        },
        "relationships": {},
        "user_defined_functions": {},
      }
    `);
  });
  it('[CITUS]', async () => {
    const httpClient = axios.create();
    const result = await DataSource(httpClient).getDriverCapabilities('citus');
    expect(result).toMatchInlineSnapshot(`
      {
        "data_schema": {
          "supports_foreign_keys": true,
        },
        "interpolated_queries": {},
        "mutations": {
          "delete": {},
          "insert": {},
          "update": {},
        },
        "queries": {
          "foreach": {},
        },
        "relationships": {},
        "user_defined_functions": {},
      }
    `);
  });
  it('[MSSQL]', async () => {
    const httpClient = axios.create();
    const result = await DataSource(httpClient).getDriverCapabilities('mssql');
    expect(result).toMatchInlineSnapshot(`
      {
        "data_schema": {
          "supports_foreign_keys": true,
        },
        "interpolated_queries": {},
        "mutations": {
          "delete": {},
          "insert": {},
          "update": {},
        },
        "queries": {
          "foreach": {},
        },
        "relationships": {},
        "user_defined_functions": {},
      }
    `);
  });
  it('[BIGQUERY]', async () => {
    const httpClient = axios.create();
    const result = await DataSource(httpClient).getDriverCapabilities(
      'bigquery'
    );
    expect(result).toMatchInlineSnapshot(`
      {
        "data_schema": {
          "supports_foreign_keys": false,
        },
        "interpolated_queries": {},
        "queries": {
          "foreach": {},
        },
        "relationships": {},
      }
    `);
  });
});
