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
import oracleLogo from './oracle.webp';
import mongodbLogo from './mongodb.svg';

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
  mysql8: mysqlLogo,
  sqlite: sqliteLogo,
  mariadb: mariadbLogo,
  oracle: oracleLogo,
  mongo: mongodbLogo,
};

export default dbLogos;
