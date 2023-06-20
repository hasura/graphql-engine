// Copyright IBM Corp. 2017,2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

'use strict';

let server; // holds server object for shutdown

/**
 * Starts the server at the given port
 */
function startServer(PORT) {
  const express = require('express');
  const app = express();

  const bodyParser = require('body-parser');
  app.use(bodyParser.json());

  app.get('/api/user', (req, res) => {
    res.send({
      name: 'Arlene L McMahon',
    });
  });

  app.get('/api/user2', (req, res) => {
    res.send({
      name: 'William B Ropp',
    });
  });

  return new Promise(resolve => {
    server = app.listen(PORT, () => {
      console.log(`Example API accessible on port ${PORT}`);
      resolve();
    });
  });
}

/**
 * Stops server.
 */
function stopServer() {
  return new Promise(resolve => {
    server.close(() => {
      console.log(`Stopped API server`);
      resolve();
    });
  });
}

// If run from command line, start server:
if (require.main === module) {
  startServer(3002);
}

module.exports = {
  startServer,
  stopServer,
};
