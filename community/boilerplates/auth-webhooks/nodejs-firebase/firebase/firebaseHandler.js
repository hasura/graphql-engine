var express = require('express');
var firebaseRouter = express.Router();
var admin = require('firebase-admin');
var serviceAccount = require('./config.js');
var error = null;
// Initialize the Firebase admin SDK with your service account credentials
if (serviceAccount) {
  try {
    admin.initializeApp({
      credential: admin.credential.cert(JSON.parse(serviceAccount))
    });
  } catch (e) {
    error = e;
  }
}

firebaseRouter.route("/webhook").get((request, response) => {
  // Throw 500 if firebase is not configured
  if (!serviceAccount) {
    response.status(500).send('Firebase not configured');
    return;
  }
  // Check for errors initializing firebase SDK
  if (error) {
    response.status(500).send('Invalid firebase configuration');
    return;
  }
  // Get authorization headers
  var authHeaders = request.get('Authorization');
  // Send anonymous role if there are no auth headers
  if (!authHeaders) {
    response.json({'x-hasura-role': 'anonymous'});
    return;
  } else {
    // Validate the received id_token
    var idToken = extractToken(authHeaders);
    console.log(idToken);
    admin.auth().verifyIdToken(idToken)
      .then((decodedToken) => {
        var hasuraVariables = {
          'X-Hasura-User-Id': decodedToken.uid,
          'X-Hasura-Role': 'user'
        };
        console.log(hasuraVariables); // For debug
        // Send appropriate variables
        response.json(hasuraVariables);
      })
      .catch((e) => {
        // Throw authentication error
        console.log(e);
        response.json({'x-hasura-role': 'anonymous'});
      });
  }
});

const extractToken = (bearerToken) => {
  const regex = /^(Bearer) (.*)$/g;
  const match = regex.exec(bearerToken);
  if (match && match[2]) {
    return match[2];
  }
  return null;
}

module.exports = firebaseRouter;
