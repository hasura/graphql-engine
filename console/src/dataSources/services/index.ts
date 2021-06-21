import { postgres } from './postgresql';
import { mysql } from './mysql';
import { mssql } from './mssql';
import { bigquery } from './bigquery';
import { citus } from './citus';

export const services = { postgres, mysql, mssql, bigquery, citus };
