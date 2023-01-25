import { postgres } from './postgresql';
import { alloy } from './alloydb';
import { mysql } from './mysql';
import { mssql } from './mssql';
import { bigquery } from './bigquery';
import { citus } from './citus';
import { cockroach } from './cockroach';

export const services = {
  postgres,
  mysql,
  mssql,
  bigquery,
  citus,
  cockroach,
  alloy,
};
