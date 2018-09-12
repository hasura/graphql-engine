var express = require('express');
var auth0Router = express.Router();
var requestClient = require('request');
var auth0Domain = process.env.AUTH_ZERO_DOMAIN;
/*
  Auth webhook handler for auth0
  Flow:
  1) Expects access_token to be sent as 'Authorization: Bearer <access-token>
  2) Verified access_token by fetching /userinfo endpoint from auth0

  Usage:
  1) From your application, when you call Hasura's GraphQL APIs remember to send the access_token from auth0 as an authorization header
  2) Replace the url (https://test-hasura.auth0.com/userinfo) in the code below with your own auth0 app url
*/

auth0Router.route('/webhook').get((request, response) => {
  // Throw 500 if auth0 domain is not configured
  if (!auth0Domain) {
    response.status(500).send('Auth0 domain not configured');
    return;
  }

  var token = request.get('Authorization');

  if (!token) {
    response.json({'x-hasura-role': 'anonymous'});
    return;
  } else {
    // Fetch information about this user from
    // auth0 to validate this token
    // NOTE: Replace the URL with your own auth0 app url
    var options = {
      url: `https://${auth0Domain}/userinfo`,
      headers: {
        Authorization: token,
        'Content-Type': 'application/json'
      }
    };

    requestClient(options, (err, res, body) => {
      if (!err && res.statusCode == 200) {
        var userInfo = JSON.parse(body);
        console.log(userInfo); //debug
        var hasuraVariables = {
          'X-Hasura-User-Id': userInfo.sub,
          'X-Hasura-Role': 'user'
        };
        console.log(hasuraVariables); // For debug
        response.json(hasuraVariables);
      } else {
        // Error response from auth0
        console.log(err, res, body);
        response.json({'x-hasura-role': 'anonymous'});
        return;
      }
    });
  }
});

module.exports = auth0Router;
