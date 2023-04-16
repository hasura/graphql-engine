import postgresLogo from './postgres.webp';
import googleLogo from './google.webp';
import microsoftLogo from './microsoft.webp';
import citusLogo from './citus.webp';
import cockroachLogo from './cockroach.webp';
import amazonLogo from './amazon.webp';
import snowflakeLogo from './snowflake.webp';
import defaultDbLogo from './default.svg';
import mysqlLogo from './mysql.webp';
import sqliteLogo from './sqlite.webp';
import mariadbLogo from './mariadb.webp';

const dbLogos: Record<string, string> = {
  pg: postgresLogo,
  postgres: postgresLogo,
  alloy: googleLogo,
  citus: citusLogo,
  cockroach: cockroachLogo,
  mssql: microsoftLogo,
  bigquery: googleLogo,
  snowflake: snowflakeLogo,
  athena: amazonLogo,
  default: defaultDbLogo,
  mysqlgdc: mysqlLogo,
  sqlite: sqliteLogo,
  mariadb: mariadbLogo,
};

export default dbLogos;
