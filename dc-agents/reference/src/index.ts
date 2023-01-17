import Fastify from 'fastify';
import FastifyCors from '@fastify/cors';
import { filterAvailableTables, getSchema, getTable, loadStaticData, StaticData } from './data';
import { queryData } from './query';
import { getConfig } from './config';
import { capabilitiesResponse } from './capabilities';
import { CapabilitiesResponse, SchemaResponse, QueryRequest, QueryResponse, DatasetDeleteResponse, DatasetPostRequest, DatasetGetResponse, DatasetPostResponse } from '@hasura/dc-api-types';
import { cloneDataset, deleteDataset, getDataset } from './datasets';

const port = Number(process.env.PORT) || 8100;
const server = Fastify({ logger: { prettyPrint: true } });
let staticData : Record<string, StaticData> = {};

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
  // Prefix '$' to disambiguate from default datasets.
  const dbName = config.db ? `$${config.db}` : '@default';
  const data = filterAvailableTables(staticData[dbName], config);
  return queryData(getTable(data, config), request.body);
});

// Methods on dataset resources.
// 
// Examples:
// 
// > curl -H 'content-type: application/json' -XGET localhost:8100/datasets/ChinookData
// {"exists": true}
// 
server.get<{ Params: { name: string, }, Reply: DatasetGetResponse }>("/datasets/templates/:name", async (request, _response) => {
  server.log.info({ headers: request.headers, query: request.body, }, "datasets.templates.get");
  return getDataset(request.params.name);
});

// > curl -H 'content-type: application/json' -XPOST localhost:8100/datasets/foo -d '{"from": "ChinookData"}'
// {"config":{"db":"$foo"}}
// 
server.post<{ Params: { name: string, }, Body: DatasetPostRequest, Reply: DatasetPostResponse }>("/datasets/clones/:name", async (request, _response) => {
  server.log.info({ headers: request.headers, query: request.body, }, "datasets.clones.post");
  return cloneDataset(staticData, request.params.name, request.body);
});

// > curl -H 'content-type: application/json' -XDELETE 'localhost:8100/datasets/foo'
// {"message":"success"}
// 
server.delete<{ Params: { name: string, }, Reply: DatasetDeleteResponse }>("/datasets/clones/:name", async (request, _response) => {
  server.log.info({ headers: request.headers, query: request.body, }, "datasets.clones.delete");
  return deleteDataset(staticData, request.params.name);
});

server.get("/health", async (request, response) => {
  server.log.info({ headers: request.headers, query: request.body, }, "health.request");
  response.statusCode = 204;
});

process.on('SIGINT', () => {
  server.log.info("interrupted");
  process.exit(0);
});

const start = async () => {
  try {
    staticData = {'@default' : await loadStaticData("ChinookData.xml.gz")};
    await server.listen(port, "0.0.0.0");
  }
  catch (err) {
    server.log.fatal(err);
    process.exit(1);
  }
};
start();
