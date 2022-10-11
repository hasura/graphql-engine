// Sample webhook showing what a hasura auth webhook looks like

// init project
var express = require('express');
var app = express();
var requestClient = require('request');
var port = process.env.PORT || 3000;

app.get('/', (req, res) => {
    res.send('Webhooks are running');
});

// Firebase handler
var firebaseRouter = require('./firebase/firebaseHandler');
app.use('/firebase', firebaseRouter);

// listen for requests :)
var listener = app.listen(port, function () {
  console.log('Your app is listening on port ' + port);
});
