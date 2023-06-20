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
  app.use(bodyParser.text());
  app.use(bodyParser.json());

  const Authors = {
    arlene: {
      name: 'Arlene L McMahon',
      masterpieceTitle: 'software',
    },
    will: {
      name: 'William B Ropp',
      masterpieceTitle: '',
    },
    johnny: {
      name: 'John C Barnes',
      masterpieceTitle: '',
    },
    heather: {
      name: 'Heather J Tate',
      masterpieceTitle: '',
    },
  };

  const Books = {
    software: {
      title: 'The OpenAPI-to-GraphQL Cookbook',
      authorName: 'arlene',
    },
    frog: {
      title: 'One Frog, Two Frog, Red Frog, Blue Frog',
      authorName: 'will',
    },
    history: {
      title: 'A history on history',
      authorName: 'will',
    },
  };

  const NextWorks = {
    arlene: {
      title: 'OpenAPI-to-GraphQL for Power Users',
      authorName: 'arlene',
    },
    johnny: {
      title: 'A one, a two, a one two three four!',
      authorName: 'johnny',
    },
    heather: {
      title: 'What did the baby computer say to the father computer? Data.',
      authorName: 'heather',
    },
  };

  const Auth = {
    arlene: {
      username: 'arlene123',
      password: 'password123',
      accessToken: 'abcdef',
    },
    will: {
      username: 'catloverxoxo',
      password: 'IActuallyPreferDogs',
      accessToken: '123456',
    },
    johnny: {
      username: 'johnny',
      password: 'password',
      accessToken: 'xyz',
    },
    heather: {
      username: 'cccrulez',
      password: 'johnnyisabully',
      accessToken: 'ijk',
    },
  };

  const authMiddleware = (req, res, next) => {
    if ('authorization' in req.headers) {
      const tokenizedAuth = req.headers.authorization.split(' ');

      if (tokenizedAuth.length == 2) {
        const authType = tokenizedAuth[0];
        const authValue = tokenizedAuth[1];

        if (authType == 'Basic') {
          // Decode username and password
          const decoded = new Buffer.from(authValue, 'base64')
            .toString('utf8')
            .split(':');

          if (decoded.length === 2) {
            const credentials = {
              username: decoded[0],
              password: decoded[1],
            };

            for (let user in Auth) {
              if (
                Auth[user].username === credentials.username &&
                Auth[user].password === credentials.password
              ) {
                return next();
              }
            }
          } else {
            res.status(401).send({
              message:
                'Basic Auth expects a single username and a single password',
            });
          }
        } else if (authType == 'Bearer') {
          if (authValue == 'master-bearer-token') {
            return next();
          }
        }
      }
    } else if ('access_token' in req.headers) {
      for (let user in Auth) {
        if (Auth[user].accessToken === req.headers.access_token) {
          return next();
        }
      }
    } else if ('cookie' in req.headers) {
      for (let user in Auth) {
        if (Auth[user].accessToken === req.headers.cookie.split('=')[1]) {
          return next();
        }
      }
    } else if ('access_token' in req.query) {
      for (let user in Auth) {
        if (Auth[user].accessToken === req.query.access_token) {
          return next();
        }
      }
    } else {
      res.status(401).send({
        message: 'Unknown/missing credentials',
      });
    }

    res.status(401).send({
      message: 'Incorrect credentials',
    });
  };

  app.get('/api/authors/:authorId', (req, res) => {
    res.send(Authors[req.params.authorId]);
  });

  app.get('/api/books/:bookId', (req, res) => {
    res.send(Books[req.params.bookId]);
  });

  app.get('/api/nextWorks/:authorId', authMiddleware, (req, res) => {
    res.send(NextWorks[req.params.authorId]);
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
  startServer(3003);
}

module.exports = {
  startServer,
  stopServer,
};
