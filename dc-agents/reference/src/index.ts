import Fastify from 'fastify';
import FastifyCors from '@fastify/cors';
import { filterAvailableTables, getSchema, getTable, loadStaticData, StaticData } from './data';
import { queryData } from './query';
import { getConfig } from './config';
import { capabilitiesResponse } from './capabilities';
import { CapabilitiesResponse, SchemaResponse, QueryRequest, QueryResponse, DatasetGetTemplateResponse, DatasetCreateCloneRequest, DatasetCreateCloneResponse, DatasetDeleteCloneResponse, SchemaRequest } from '@hasura/dc-api-types';
import { cloneDataset, defaultDbStoreName, deleteDataset, getDataset, getDbStoreName } from './datasets';

const port = Number(process.env.PORT) || 8100;
const server = Fastify({ logger: { transport: { target: 'pino-pretty' } }});
let staticData : Record<string, StaticData> = {};

server.register(FastifyCors, {
  // Accept all origins of requests. This must be modified in
  // a production setting to be specific allowable list
  // See: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Origin
  origin: true,
  methods: ["GET", "POST", "OPTIONS"],
  allowedHeaders: ["X-Hasura-DataConnector-Config", "X-Hasura-DataConnector-SourceName"]
});

// This is a hack to get Fastify to parse bodies on /schema GET requests
// We basically trick its code into thinking the request is actually a POST
// request so it doesn't skip parsing request bodies.
server.addHook("onRequest", async(request, reply) => {
  if (request.routerPath === "/schema")
    request.raw.method = "POST"
})

server.get<{ Reply: CapabilitiesResponse }>("/capabilities", async (request, _response) => {
  server.log.info({ headers: request.headers, query: request.body, }, "capabilities.request");
  return capabilitiesResponse;
});

server.post<{ Body: SchemaRequest | undefined, Reply: SchemaResponse }>("/schema", async (request, _response) => {
  server.log.info({ headers: request.headers, query: request.body, }, "schema.request");
  const config = getConfig(request);
  return getSchema(staticData, config, request.body);
});

server.post<{ Body: QueryRequest, Reply: QueryResponse }>("/query", async (request, _response) => {
  server.log.info({ headers: request.headers, query: request.body, }, "query.request");

  const config = getConfig(request);
  const dbStoreName = config.db ? getDbStoreName(config.db) : defaultDbStoreName;

  if (!(dbStoreName in staticData))
    throw new Error(`Cannot find database ${config.db}`);

  const data = filterAvailableTables(staticData[dbStoreName], config);
  return queryData(getTable(data, config), request.body);
});

// Methods on dataset resources.
//
// Examples:
//
// > curl -H 'content-type: application/json' -XGET localhost:8100/datasets/templates/Chinook
// {"exists": true}
//
server.get<{ Params: { name: string, }, Reply: DatasetGetTemplateResponse }>("/datasets/templates/:name", async (request, _response) => {
  server.log.info({ headers: request.headers, query: request.body, }, "datasets.templates.get");
  return getDataset(request.params.name);
});

// > curl -H 'content-type: application/json' -XPOST localhost:8100/datasets/clones/foo -d '{"from": "Chinook"}'
// {"config":{"db":"$foo"}}
//
server.post<{ Params: { name: string, }, Body: DatasetCreateCloneRequest, Reply: DatasetCreateCloneResponse }>("/datasets/clones/:name", async (request, _response) => {
  server.log.info({ headers: request.headers, query: request.body, }, "datasets.clones.post");
  return cloneDataset(staticData, request.params.name, request.body);
});

// > curl -H 'content-type: application/json' -XDELETE 'localhost:8100/datasets/clones/foo'
// {"message":"success"}
//
server.delete<{ Params: { name: string, }, Reply: DatasetDeleteCloneResponse }>("/datasets/clones/:name", async (request, _response) => {
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
    staticData = {[defaultDbStoreName]: await loadStaticData("Chinook")};
    await server.listen({port: port, host: "0.0.0.0"});
  }
  catch (err) {
    server.log.fatal(err);
    process.exit(1);
  }
};
start();
