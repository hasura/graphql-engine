// Sample webhook showing what a hasura auth webhook looks like

// init project
var express = require('express');
var app = express();
var port = process.env.PORT || 8080;
var jwt = require('jsonwebtoken');
var fs = require('fs');

const AUTH0_JWT_SECRET = process.env.AUTH0_JWT_SECRET ? process.env.AUTH0_JWT_SECRET : fs.readFileSync('./graphql-tutorials.pem');
const AUTH0_ISSUER = 'https://graphql-tutorials.auth0.com/';
const CUSTOM_JWT_SECRET = process.env.CUSTOM_JWT_SECRET;
const CUSTOM_ISSUER = 'https://learn.hasura.io/';

app.get('/', (req, res) => {
  res.send('Webhooks are running');
});

app.get('/webhook', (request, response) => {
  // Extract token from request
  var token = request.get('Authorization').replace('Bearer ', '');
  let issuer;
  try {
    var decoded = jwt.decode(token);
    issuer = decoded.iss;
  } catch(e) {
    console.log(e);
    response.status(400);
    response.send('invalid token');
  }

  let hasuraVariables = {'X-Hasura-Role': 'user'};
  try {
    // check if auth0 or custom server
    if(issuer === AUTH0_ISSUER) {
      var verify = jwt.verify(token, AUTH0_JWT_SECRET, {algorithm: 'RS256'});
      hasuraVariables['X-Hasura-User-Id'] = verify['https://hasura.io/jwt/claims']['x-hasura-user-id']
      response.json(hasuraVariables);
    } else if(issuer === CUSTOM_ISSUER) {
      var verify = jwt.verify(token, CUSTOM_JWT_SECRET, {algorithm: 'RS256'});
      hasuraVariables['X-Hasura-User-Id'] = verify['https://hasura.io/jwt/claims']['x-hasura-user-id']
      response.json(hasuraVariables);
     } else {
      response.status(400);
      response.send('invalid issuer');
    }
  } catch(e) {
    console.log(e);
    response.status(500);
    response.send('error ' + e);
  }

});

// listen for requests :)
app.set('host', '0.0.0.0');
var listener = app.listen(port, function () {
  console.log('Your app is listening on port ' + port);
});
