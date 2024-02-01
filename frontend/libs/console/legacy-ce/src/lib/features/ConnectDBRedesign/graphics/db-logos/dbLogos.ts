import postgresLogo from './postgres.webp';
import googleLogo from './google.webp';
import microsoftLogo from './microsoft.webp';
import citusLogo from './citus.webp';
import cockroachLogo from './cockroach.webp';
import amazonLogo from './amazon.webp';
import snowflakeLogo from './snowflake.webp';
import mysqlLogo from './mysql.webp';
import sqliteLogo from './sqlite.webp';
import mariadbLogo from './mariadb.webp';
import oracleLogo from './oracle.webp';
import mongodbLogo from './mongodb.svg';
import clickhouseLogo from './clickhouse.svg';
import trinoLogo from './trino.svg';

export const dbLogos: Record<string, string> = {
  pg: postgresLogo,
  postgres: postgresLogo,
  alloy: googleLogo,
  citus: citusLogo,
  cockroach: cockroachLogo,
  mssql: microsoftLogo,
  bigquery: googleLogo,
  snowflake: snowflakeLogo,
  athena: amazonLogo,
  mysql8: mysqlLogo,
  mysql: mysqlLogo,
  sqlite: sqliteLogo,
  mariadb: mariadbLogo,
  oracle: oracleLogo,
  mongo: mongodbLogo,
  mongodb: mongodbLogo,
  clickhouse: clickhouseLogo,
  trino: trinoLogo,
};
