// Sample webhook showing what a hasura auth webhook looks like

// init project
var express = require('express');
var app = express();
var requestClient = require('request');
var port = process.env.PORT || 3000;

/* A simple sample
   Flow:
   1) Extracts token
   2) Fetches userInfo in a mock function
   3) Return hasura variables
*/
function fetchUserInfo (token, cb) {
  // This function takes a token and then makes an async
  // call to the session-cache or database to fetch
  // data that is needed for Hasura's access control rules
  cb();
}
app.get('/', (req, res) => {
  res.send('Webhooks are running');
});

app.get('/webhook', (request, response) => {
  // Extract token from request
  var token = request.get('Authorization');

  // Fetch user_id that is associated with this token
  fetchUserInfo(token, (result) => {

    // Return appropriate response to Hasura
    var hasuraVariables = {
      'X-Hasura-Role': 'user',  // result.role
      'X-Hasura-User-Id': '1'    // result.user_id
    };
    response.json(hasuraVariables);
  });
});

// listen for requests :)
var listener = app.listen(port, function () {
  console.log('Your app is listening on port ' + port);
});
