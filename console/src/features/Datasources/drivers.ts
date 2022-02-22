import { APIError } from '@/hooks/error';
import { MetadataDataSource } from '@/metadata/types';
import { UseQueryResult } from 'react-query';
import bigquery, { BigQueryDataTarget } from './bigquery';
import citus, { CitusDataTarget } from './citus';
import mssql, { MssqlDataTarget } from './mssql';
import mysql, { MysqlDataTarget } from './mysql';
import postgres, { PostgresDataTarget } from './postgres';
import { RunSQLQueryOptions } from './types';

export type DataTarget =
  | PostgresDataTarget
  | CitusDataTarget
  | MssqlDataTarget
  | BigQueryDataTarget
  | MysqlDataTarget;

export type TUseTableRelationshipsQuery = (props: {
  target: DataTarget;
  queryOptions?: RunSQLQueryOptions<string[], any>;
}) => UseQueryResult<
  {
    from: {
      table: string;
      column: string[];
    };
    to: {
      table: string;
      column: string[];
    };
  }[],
  APIError
>;

interface Driver {
  useTableRelationshipsQuery: TUseTableRelationshipsQuery;
}

type TDriversMap = {
  [key in MetadataDataSource['kind']]: Driver;
};

export const drivers: TDriversMap = {
  postgres,
  citus,
  mssql,
  bigquery,
  mysql,
};
