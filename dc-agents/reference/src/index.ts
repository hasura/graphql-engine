import Fastify from 'fastify';
import { SchemaResponse } from './types/schema';
import { ProjectedRow, QueryRequest } from './types/query';
import { filterAvailableTables, getSchema, loadStaticData } from './data';
import { queryData } from './query';
import { getConfig } from './config';
import { CapabilitiesResponse, capabilitiesResponse} from './capabilities';

const port = Number(process.env.PORT) || 8100;
const server = Fastify({ logger: { prettyPrint: true } });
let staticData = {};

server.get<{ Reply: CapabilitiesResponse }>("/capabilities", async (request, _response) => {
  server.log.info({ headers: request.headers, query: request.body, }, "capabilities.request");
  return capabilitiesResponse;
});

server.get<{ Reply: SchemaResponse }>("/schema", async (request, _response) => {
  server.log.info({ headers: request.headers, query: request.body, }, "schema.request");
  const config = getConfig(request);
  return getSchema(config);
});

server.post<{ Body: QueryRequest, Reply: ProjectedRow[] }>("/query", async (request, _response) => {
  server.log.info({ headers: request.headers, query: request.body, }, "query.request");
  const config = getConfig(request);
  const data = filterAvailableTables(staticData, config);
  return queryData(data, request.body);
});

process.on('SIGINT', () => {
  server.log.info("interrupted");
  process.exit(0);
});

const start = async () => {
  try {
    staticData = await loadStaticData();
    await server.listen(port, "0.0.0.0");
  }
  catch (err) {
    server.log.fatal(err);
    process.exit(1);
  }
};
start();
