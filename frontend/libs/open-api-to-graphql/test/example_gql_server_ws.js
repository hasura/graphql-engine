// Copyright IBM Corp. 2017,2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

'use strict';

const express = require('express');
const { graphqlHTTP } = require('express-graphql');
const { execute, printSchema, subscribe } = require('graphql');
const { createServer } = require('http');
const { SubscriptionServer } = require('subscriptions-transport-ws');
const { MQTTPubSub } = require('graphql-mqtt-subscriptions');
const { connect } = require('mqtt');
const openAPIToGraphQL = require('../dist/index');

const oas5 = require('./fixtures/example_oas5.json');

const HTTP_PORT = 3000;
const app = express();

openAPIToGraphQL
  .createGraphQLSchema(oas5, {
    fillEmptyResponses: true,
    createSubscriptionsFromCallbacks: true,
  })
  .then(({ schema, report }) => {
    console.log(JSON.stringify(report, null, 2));
    const myGraphQLSchema = printSchema(schema);
    console.log(myGraphQLSchema);

    const client = connect(`mqtt://localhost:1885`, {
      keepalive: 60,
      reschedulePings: true,
      protocolId: 'MQTT',
      protocolVersion: 4,
      reconnectPeriod: 2000,
      connectTimeout: 5 * 1000,
      clean: true,
    });

    const pubsub = new MQTTPubSub({
      client,
    });

    app.use(
      '/graphql',
      graphqlHTTP({
        schema,
        graphiql: true,
        subscriptionsEndpoint: `ws://localhost:${HTTP_PORT}/subscriptions`,
        context: { pubsub },
      })
    );

    const server = createServer(app);

    server.listen(HTTP_PORT, () => {
      console.log(`Running a GraphQL API server at :`);
      console.log(`http://localhost:${HTTP_PORT}/graphql`);
      console.log(`ws://localhost:${HTTP_PORT}/subscriptions`);

      new SubscriptionServer(
        {
          execute,
          subscribe,
          schema,
          onConnect: (params, socket, ctx) => {
            // Add pubsub to subscribe context
            return { pubsub };
          },
        },
        {
          server,
          path: '/subscriptions',
        }
      );
    });
  })
  .catch(err => {
    console.log(err);
  });
