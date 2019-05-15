const {query} = require('graphqurl');
const fetch = require('node-fetch');
const colors = require('colors/safe');

const complexQuery = `
query {
  f2gt_Album (order_by:{_id:asc}){
    _id
    f2gt_Track (order_by: {_id:asc}) {
      _id
      Name
    }
    f2gt_Artist {
      Name
      f2gt_Album (order_by: {_id:desc}){
        _id
        Title
        f2gt_Track (order_by: {Name:asc}){
          Name
          Composer
        }
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
      response.data.f2gt_Album[0]._id === '1' &&
      response.data.f2gt_Album[0].f2gt_Track[1]._id === '10' &&
      response.data.f2gt_Album[0].f2gt_Artist.Name === 'AC/DC' &&
      response.data.f2gt_Album[0].f2gt_Artist.f2gt_Album[0].Title === 'Let There Be Rock' &&
      response.data.f2gt_Album[0].f2gt_Artist.f2gt_Album[0].f2gt_Track[0].Name === 'Bad Boy Boogie'
    ) {
      let sqlString = '';
      ['Album', 'Artist', 'Tracks'].forEach(t => {
        sqlString += `drop table public."f2gt_${t}" cascade;`;
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
        console.log(colors.green('✔︎ data-sets/chinook_nested.json: Test passed'));
        process.exit();
      }).catch(() => {
        process.exit();
      });
    } else {
      console.log(colors.red('✖ data-sets/chinook_nested.json: Test failed. Unexpected response.'));
      process.exit();
    }
  }).catch(e => {
    console.log(colors.red('✖ data-sets/chinook_nested.json: Test failed. Unexpected response.'));
    console.log(JSON.stringify(e, null, 2));

    process.exit();
  });
};

verifyDataImport();
