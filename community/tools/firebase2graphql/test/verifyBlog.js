const {query} = require('graphqurl');
const fetch = require('node-fetch');
const colors = require('colors/safe');

const complexQuery = `
query {
  f2g_test_posts (order_by: title_asc) {
    title
  }
  f2g_test_users (order_by: username_asc) {
    username
  }
  f2g_test_user_posts (order_by:title_asc){
    author
    title
  }
}
`;

const verifyDataImport = () => {
  query({
    query: complexQuery,
    endpoint: `${process.env.TEST_HGE_URL}/v1alpha1/graphql`,
    headers: {'x-hasura-access-key': process.env.TEST_X_HASURA_ACCESS_KEY},
  }).then(response => {
    if (
      response.data.f2g_test_posts[0].title === 'My first post' &&
      response.data.f2g_test_users[0].username === 'Eena' &&
      response.data.f2g_test_user_posts[1].title === 'Whatta proaaa'
    ) {
      let sqlString = '';
      ['f2g_test_users', 'f2g_test_posts', 'f2g_test_user_posts'].forEach(t => {
        sqlString += `drop table public."${t}" cascade;`;
      });
      fetch(
        `${process.env.TEST_HGE_URL}/v1/query`,
        {
          method: 'POST',
          headers: {'x-hasura-access-key': process.env.TEST_X_HASURA_ACCESS_KEY},
          body: JSON.stringify({
            type: 'run_sql',
            args: {
              sql: sqlString,
              cascade: true,
            },
          }),
        }
      ).then(() => {
        console.log(colors.green('✔︎ data-sets/blog.json: Test passed'));
        process.exit();
      }).catch(() => {
        process.exit();
      });
    } else {
      console.log(colors.red('✖ data-sets/blog.json: Test failed. Unexpected response.'));
      console.log(response.data);
      process.exit();
    }
  });
};

verifyDataImport();
