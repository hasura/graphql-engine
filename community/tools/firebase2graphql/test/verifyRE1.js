const {query} = require('graphqurl');
const fetch = require('node-fetch');
const colors = require('colors/safe');

const complexQuery = `
query {
  f2g_test_Authors (order_by: {Name:asc}) {
    _id
    Name
    f2g_Articles (order_by: {Title:asc}, where: { IsUnpublished: { _eq: true}}) {
      Title
      f2g_test_Comments (order_by: {Date:asc}) {
        Body
        Date
      }
    }
  }
}
`;

const verifyDataImport = () => {
  query({
    query: complexQuery,
    endpoint: `${process.env.TEST_HGE_URL}/v1/graphql`,
    headers: {'x-hasura-admin-secret': process.env.TEST_X_HASURA_ADMIN_SECRET},
  }).then(response => {
    if (
      response.data &&
      response.data.f2g_test_Authors[0].f2g_Articles.length === 0 &&
      response.data.f2g_test_Authors[1].f2g_Articles[0].f2g_test_Comments[0].Body === 'Comment1'
    ) {
      let sqlString = '';
      ['Articles', 'Authors', 'Comments'].forEach(t => {
        sqlString += `drop table public."f2g_test_${t}" cascade;`;
      });
      fetch(
        `${process.env.TEST_HGE_URL}/v1/query`,
        {
          method: 'POST',
          headers: {'x-hasura-admin-secret': process.env.TEST_X_HASURA_ADMIN_SECRET},
          body: JSON.stringify({
            type: 'run_sql',
            args: {
              sql: sqlString,
              cascade: true,
            },
          }),
        }
      ).then(() => {
        console.log(colors.green('✔︎ data-sets/readme-example-1.json: Test passed'));
        process.exit();
      }).catch(() => {
        process.exit();
      });
    } else {
      console.log(colors.red('✖ data-sets/readme-example-1.json: Test failed. Unexpected response.'));
      process.exit();
    }
  }).catch(e => {
    console.log(colors.red('✖ data-sets/readme-example-1.json: Test failed. Unexpected response.'));
    console.log(JSON.stringify(e, null, 2));

    process.exit();
  });
};

verifyDataImport();
