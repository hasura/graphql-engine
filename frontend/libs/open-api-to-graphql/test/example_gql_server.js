// Copyright IBM Corp. 2017,2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

'use strict';

const express = require('express');
const graphql = require('graphql');
const { graphqlHTTP } = require('express-graphql');
const app = express();
const openAPIToGraphQL = require('../dist/index');

// const oas = require('./fixtures/example_oas.json')
// const oas = require('./fixtures/example_oas2.json')
// const oas = require('./fixtures/example_oas3.json')
// const oas = require('./fixtures/example_oas4.json')
// const oas = require('./fixtures/example_oas5.json')
// const oas = require('./fixtures/example_oas6.json')
const oas = require('./fixtures/example_oas7.json');
// const oas = require('./fixtures/example_oas8.json')
// const oas = require('./fixtures/file_upload.json')

// const oas = require('./fixtures/github.json')
// const oas = require('./fixtures/instagram.json')
// const oas = require('./fixtures/ibm_language_translator.json')
// const oas = require('./fixtures/government_social_work.json')
// const oas = require('./fixtures/weather_underground.json')
// const oas = require('./fixtures/stripe.json')

// const yamljs = require('yamljs')
// const fs = require('fs')
// // requires Box API from API Guru
// const oas = yamljs.parse(fs.readFileSync('../tmp/APIs/box.com/content/2.0/swagger.yaml', 'utf8'))

openAPIToGraphQL
  .createGraphQLSchema(oas, {
    fillEmptyResponses: true,
  })
  .then(({ schema, report }) => {
    console.log(JSON.stringify(report, null, 2));

    app.use(
      '/graphql',
      graphqlHTTP({
        schema: schema,
        graphiql: true,
      })
    );

    app.listen(3000, () => {
      console.log('GraphQL accessible at: http://localhost:3000/graphql');
    });
  })
  .catch(err => {
    console.log(err);
  });
