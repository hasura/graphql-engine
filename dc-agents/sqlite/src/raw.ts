import { Config } from "./config";
import { connect, SqlLogger } from './db';
import { RawRequest, RawResponse } from '@hasura/dc-api-types';

export async function runRawOperation(config: Config, sqlLogger: SqlLogger, query: RawRequest): Promise<RawResponse> {
  const db = connect(config, sqlLogger);
  const [results, metadata] = await db.query(query.query);

  return {
    rows: results as Array<Record<string, any>>
  };
};
