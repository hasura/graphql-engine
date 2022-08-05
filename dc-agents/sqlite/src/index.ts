import Fastify                                         from 'fastify';
import FastifyCors                                     from '@fastify/cors';
import { getSchema }                                   from './schema';
import { queryData }                                   from './query';
import { getConfig }                                   from './config';
import { CapabilitiesResponse, capabilitiesResponse}   from './capabilities';
import { connect }                                     from './db';
import { stringToBool }                                from './util';
import { QueryResponse, SchemaResponse, QueryRequest } from './types';
import * as fs                                         from 'fs'

const port = Number(process.env.PORT) || 8100;
const server = Fastify({ logger: { prettyPrint: true } });

if(stringToBool(process.env['PERMISSIVE_CORS'])) {
  // See: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Origin
  server.register(FastifyCors, {
    origin: true,
    methods: ["GET", "POST", "OPTIONS"],
    allowedHeaders: ["X-Hasura-DataConnector-Config", "X-Hasura-DataConnector-SourceName"]
  });
}

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
  return queryData(config, request.body);
});

server.get("/health", async (request, response) => {
  const config = getConfig(request);
  response.type('application/json');

  if(config.db == null) {
    server.log.info({ headers: request.headers, query: request.body, }, "health.request");
    // response.statusCode = 204;
    return { "status": "ok" };
  } else {
    server.log.info({ headers: request.headers, query: request.body, }, "health.db.request");
    const db = connect(config);
    const [r, m] = await db.query('select 1 where 1 = 1');
    if(r && JSON.stringify(r) == '[{"1":1}]') {
      response.statusCode = 204;
      return { "status": "ok" };
    } else {
      response.statusCode = 500;
      return { "error": "problem executing query", "query_result": r };
    }
  }
});

server.get("/swagger.json", async (request, response) => {
  fs.readFile('src/types/agent.openapi.json', (err, fileBuffer) => {
    response.type('application/json');
    response.send(err || fileBuffer)
  })
})

server.get("/", async (request, response) => {
  response.type('text/html');
  return `<!DOCTYPE html>
    <html>
      <head>
        <title>Hasura Data Connectors SQLite Agent</title>
      </head>
      <body>
        <h1>Hasura Data Connectors SQLite Agent</h1>
        <p>See <a href="https://github.com/hasura/graphql-engine#hasura-graphql-engine">
          the GraphQL Engine repository</a> for more information.</p>
        <ul>
          <li><a href="/">GET / - This Page</a>
          <li><a href="/capabilities">GET /capabilities - Capabilities Metadata</a>
          <li><a href="/schema">GET /schema - Agent Schema</a>
          <li><a href="/query">POST /query - Query Handler</a>
          <li><a href="/health">GET /health - Healthcheck</a>
          <li><a href="/swagger.json">GET /swagger.json - Swagger JSON</a>
        </ul>
      </body>
    </html>
  `;
})

process.on('SIGINT', () => {
  server.log.info("interrupted");
  process.exit(0);
});

const start = async () => {
  try {
    await server.listen(port, "0.0.0.0");
  }
  catch (err) {
    server.log.fatal(err);
    process.exit(1);
  }
};
start();
