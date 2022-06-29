import Fastify from 'fastify';
import FastifyCors from '@fastify/cors';
import { filterAvailableTables, getSchema, loadStaticData } from './data';
import { queryData } from './query';
import { getConfig } from './config';
import { capabilitiesResponse } from './capabilities';
import { CapabilitiesResponse, SchemaResponse, QueryRequest, QueryResponse } from './types';

const port = Number(process.env.PORT) || 8100;
const server = Fastify({ logger: { prettyPrint: true } });
let staticData = {};

server.register(FastifyCors, {
  // Accept all origins of requests. This must be modified in
  // a production setting to be specific allowable list
  // See: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Origin
  origin: true,
  methods: ["GET", "POST", "OPTIONS"],
  allowedHeaders: ["X-Hasura-DataConnector-Config", "X-Hasura-DataConnector-SourceName"]
});

server.get<{ Reply: CapabilitiesResponse }>("/capabilities", async (request, _response) => {
  server.log.info({ headers: request.headers, query: request.body, }, "capabilities.request");
  return capabilitiesResponse;
});

server.get<{ Reply: SchemaResponse }>("/schema", async (request, _response) => {
  server.log.info({ headers: request.headers, query: request.body, }, "schema.request");
  const config = getConfig(request);
  return getSchema(config);
});

server.post<{ Body: QueryRequest, Reply: QueryResponse }>("/query", async (request, _response) => {
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
