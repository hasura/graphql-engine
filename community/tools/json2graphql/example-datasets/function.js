const fetch = require('node-fetch');

const db = async () => {
  const response = await fetch(
    'https://bazookaand.herokuapp.com/v1/graphql',
    {
      method: 'POST',
      headers: {
        'x-hasura-access-key': 'advancedbitch'
      },
      body: JSON.stringify({
        query: `
          query {
            items {
              id
              item
              order_id
            }
            users {
              id
              name
              balance
            }
          }
        `,
      })
    }
  );
  const respObj = await response.json();
  return respObj.data;
}

module.exports = db;