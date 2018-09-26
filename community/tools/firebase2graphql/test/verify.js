const {query} = require('graphqurl');
const fetch = require('node-fetch');
const colors = require('colors/safe');

const complexQuery = `
query {
  f2g_test_Album (
    order_by:_id_asc
  ){
    _id
    f2g_test_Album_artist {
      Name
      ArtistId
    }
    f2g_test_Album_tracks (
      order_by: Name_asc
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
    endpoint: `${process.env.TEST_HGE_URL}/v1alpha1/graphql`,
    headers: {'x-hasura-access-key': process.env.TEST_X_HASURA_ACCESS_KEY},
  }).then(response => {
    if (
      response.data.f2g_test_Album[0].f2g_test_Album_artist.ArtistId === 1 &&
      response.data.f2g_test_Album[0].f2g_test_Album_tracks[0].Name === 'Breaking The Rules'
    ) {
      let sqlString = '';
      ['Album', 'Album_artist', 'Album_tracks'].forEach(t => {
        sqlString += `drop table public."f2g_test_${t}" cascade;`;
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
        console.log(colors.green('✔︎ Test passed'));
        process.exit();
      }).catch(() => {
        process.exit();
      });
    } else {
      console.log(colors.red('✖ Test failed. Unexpected response.'));
      console.log(response.data);
      process.exit();
    }
  });
};

verifyDataImport();
