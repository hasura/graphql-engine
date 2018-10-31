const {query} = require('graphqurl');

const complexQuery = `
query {
  favoriteRoutes {
    routesByRoutesId {
      leaguesByLeaguesId {
        flightssByLeaguesId {
          flightCommentssByFlightsId (order_by: {users_id:asc}){
            users_id
            usersByUsersId {
              email
            }
          }
        }
      }
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
    if (response.data.favoriteRoutes[0].routesByRoutesId.leaguesByLeaguesId.flightssByLeaguesId[0].flightCommentssByFlightsId[0].usersByUsersId.email === 'osxcode@gmail.com') {
      console.log('✔︎ Test passed');
      process.exit();
    } else {
      console.log('✖ Test failed. Unexpected response.');
      console.log(response.data);
    }
  });
};

verifyDataImport();
