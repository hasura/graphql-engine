const {query} = require('graphqurl');
const fetch = require('node-fetch');
const colors = require('colors/safe');

const complexQuery = `
query {
  f2g_test_Album (
    order_by:{_id:asc}
  ){
    _id
    f2g_test_Album_artist {
      Name
      ArtistId
    }
    f2g_test_Track (
      order_by: {Name:asc}
    ) {
      Name
      Composer
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
      response.data.f2g_test_Album[0].f2g_test_Album_artist.ArtistId === 1 &&
      response.data.f2g_test_Album[0].f2g_test_Track[0].Name === 'Breaking The Rules'
    ) {
      let sqlString = '';
      ['Album', 'Album_artist', 'Track'].forEach(t => {
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
        console.log(colors.green('✔︎ data-sets/chinook.json: Test passed'));
        process.exit();
      }).catch(() => {
        process.exit();
      });
    } else {
      console.log(colors.red('✖ data-sets/chinook.json: Test failed. Unexpected response.'));
      console.log(response.data);
      process.exit();
    }
  }).catch(e => {
    console.log(colors.red('✖ data-sets/chinook.json: Test failed. Unexpected response.'));
    console.log(JSON.stringify(e, null, 2));
    process.exit();
  });
};

verifyDataImport();
