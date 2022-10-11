#!/usr/bin/env node

// Some copypasta that does an exhaustive introspection query on some graphql
// server and outputs a pretty-printed schema.
//
// Install dependencies:
//  
//    $ npm install -g axios graphql
//
// Usage, e.g.:
//
//    $ NODE_PATH=$(npm root --quiet -g) utils/dump-remote-schema.js http://localhost:8088/v1/graphql
//
// TODO whatever if there's a more appropriate way to install dependencies such
// that this script can be called from anywhere, and without littering
// everything with node_modules directories.

const { introspectionQuery, buildClientSchema, printSchema } = require('graphql');
const axios = require('axios');


if (process.argv.length != 3){
  console.log("Supply the graphql server URL as the only argument on the command line");
  process.exit(1); 
}


axios({
  url: process.argv[2], 
  method: 'post',
  headers:  { 'Content-Type': 'application/json' },
  data: {operationName: "IntrospectionQuery", query: introspectionQuery},
}).then(({data}) => {
    console.log(data);
  if (data.errors) {
    console.log(data.errors);
    console.log("\n   ^^^^^^^^^^^^^^^  OOPS GOT SOME ERRORS FROM THE SERVER  ^^^^^^^^^^^^^^^\n\n");
    // proceed anyway I guess
  }
  const schema = buildClientSchema(data.data);
  console.log(printSchema(schema));

}).catch(error => {
  console.log(error);
  console.log("\n   ^^^^^^^^^^^^^^^  OOPS GOT SOME ERRORS  ^^^^^^^^^^^^^^^\n\n");
});
