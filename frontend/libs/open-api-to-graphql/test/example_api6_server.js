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
  app.use(bodyParser.urlencoded({ extended: true }));

  app.get('/api/object', (req, res) => {
    res.send({
      data: 'object',
    });
  });

  app.get('/api/object2', (req, res) => {
    if (typeof req.headers.specialheader === 'string') {
      res.send({
        data: `object2 with special header: '${req.headers.specialheader}'`,
      });
    } else {
      res.send({
        data: 'object2',
      });
    }
  });

  app.post('/api/formUrlEncoded', (req, res) => {
    res.send(req.body);
  });

  app.get('/api/cars/:id', (req, res) => {
    res.send(`Car ID: ${req.params.id}`);
  });

  app.get('/api/cacti/:cactusId', (req, res) => {
    res.send(`Cactus ID: ${req.params.cactusId}`);
  });

  app.get(
    '/api/eateries/:eatery/breads/:breadName/dishes/:dishKey',
    (req, res) => {
      res.send(
        `Parameters combined: ${req.params.eatery} ${req.params.breadName} ${req.params.dishKey}`
      );
    }
  );

  function stringifyRussianDolls(russianDoll) {
    if (!typeof russianDoll.name === 'string') {
      return '';
    }

    if (typeof russianDoll.nestedDoll === 'object') {
      return `${russianDoll.name}, ${stringifyRussianDolls(
        russianDoll.nestedDoll
      )}`;
    } else {
      return russianDoll.name;
    }
  }

  app.get('/api/nestedReferenceInParameter', (req, res) => {
    res.send(stringifyRussianDolls(req.query.russianDoll));
  });

  app.get('/api/strictGetOperation', (req, res) => {
    if (req.headers['content-type']) {
      res
        .status(400)
        .set('Content-Type', 'text/plain')
        .send('Get request should not have Content-Type');
    } else {
      res.set('Content-Type', 'text/plain').send('Perfect!');
    }
  });

  app.get('/api/noResponseSchema', (req, res) => {
    res.set('Content-Type', 'text/plain').send('Hello world');
  });

  app.get('/api/returnNumber', (req, res) => {
    res.set('Content-Type', 'text/plain').send(req.headers.number);
  });

  app.get('/api/testLinkWithNonStringParam', (req, res) => {
    res.send({ hello: 'world' });
  });

  app.get('/api/testLinkwithNestedParam', (req, res) => {
    res.send({ nesting1: { nesting2: 5 } });
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
  startServer(3006);
}

module.exports = {
  startServer,
  stopServer,
};
