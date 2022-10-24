import { rest, RestRequest } from 'msw';
import {
  ErrorResponse,
  MetadataNetworkData,
  allNetworkData,
  NetworkDataType,
} from './networkData';

const BASE_URL = '/v2/query';
type NetworkDataKey = keyof NetworkDataType;

const getSqlTestId = (sql: string): NetworkDataKey => {
  sql = sql.trim();
  const index = sql.indexOf('\n');
  const firstLine = sql.substring(0, index);
  const match = firstLine.match(/--\s*test_id\s*=\s*([\w-]+)/);
  if (!match?.[1]) {
    throw new Error('Sql has no test id');
  }
  return match[1] as NetworkDataKey;
};

const getTestDriverKey = (type: string): keyof typeof allNetworkData => {
  const match = type.match(/(\w+)_run_sql/);
  if (!match?.[1] || match[1] === 'citus') return 'postgresNetworkData';
  return `${match[1]}NetworkData` as keyof typeof allNetworkData;
};

interface RunSQLRequest {
  type: string;
  args: {
    sql: string;
    source: string;
  };
}

export const networkStubs = {
  successWithData: rest.post(
    BASE_URL,
    (req: RestRequest<RunSQLRequest>, res, context) => {
      const testId = getSqlTestId(req.body.args.sql);
      const driverKey = getTestDriverKey(req.body.type);
      const stubBody = allNetworkData[driverKey][testId];
      return res(context.json(stubBody));
    }
  ),
  metadata: rest.post('/v1/metadata', (_, res, context) => {
    return res(context.json(JSON.parse(MetadataNetworkData.constant)));
  }),
  errorUnknownSource: rest.post(BASE_URL, (_, res, context) => {
    return res(context.status(400), context.json(ErrorResponse.unknownSource));
  }),
  errorWrongDriver: rest.post(BASE_URL, (_, res, context) => {
    return res(context.status(400), context.json(ErrorResponse.wrongDriver));
  }),
};
