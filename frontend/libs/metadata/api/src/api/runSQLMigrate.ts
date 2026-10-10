import { QualifiedDataSource, RunSQLResponse } from '@hasura/shared/types';
import { NetworkArgs } from '../types';
import { getRunSqlQuery, GetRunSqlQueryReturn } from './runSQL';

export type RunSQLMigrateArgs = {
  name: string;
  up: GetRunSqlQueryReturn[];
  down: GetRunSqlQueryReturn[];
  datasource: string;
  skip_execution?: boolean | undefined;
};

// <hasura-cli-server-host>/apis/migrate
export async function runSQLMigrate({
  url,
  fetchJson,
  body,
}: NetworkArgs & {
  body: RunSQLMigrateArgs;
}): Promise<RunSQLResponse[]> {
  return fetchJson(url, {
    method: 'POST',
    body: JSON.stringify(body),
  });
}

export const getDownQueryComments = (
  upSQLs: string[],
  source: QualifiedDataSource,
) => {
  if (!upSQLs.length) {
    return [];
  }

  const comment = [
    'Could not auto-generate a down migration.',
    'Please write an appropriate down migration for the SQL below:',
    ...upSQLs,
    '',
  ]
    .join('\n')
    // Normalize \r\n to \n and add comments before every line
    .replace(/\r?(^|\n)(?!$)/g, '$1-- ')
    // Eliminate trailing spaces
    .replace(/ +\n/g, '\n');

  return [
    getRunSqlQuery({
      sql: comment,
      source,
    }),
  ];
};
