const fetch = require('node-fetch');
const deepEqual = require('deep-equal');

const endpoint = process.argv[2];

const testQuery = {
  query: '{ author { name } }'
};

const testResponse = {
  data: {
    author: []
  }
};

fetch(endpoint, {
  method: 'post',
  body:    JSON.stringify(testQuery),
  headers: { 'Content-Type': 'application/json' },
})
  .then(res => res.json())
  .then((json) => {
    if (!deepEqual(json, testResponse)) {
      console.log('unexpected response');
      console.log('expected: ', testResponse);
      console.log('got:      ', json);
      process.exit(1);
    }
    console.log('response matching');
    process.exit(0);
  })
  .catch((err) => {
    console.error(err);
    process.exit(1);
  });
