import { postgres } from './postgresql';
import { mysql } from './mysql';
import { mssql } from './mssql';
import { bigquery } from './bigquery';

export const services = { postgres, mysql, mssql, bigquery };
