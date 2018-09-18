const {query} = require('graphqurl');

const complexQuery = `
query {
  favoriteRoutes {
    routeByRoutesId {
      leagueByLeaguesId {
        flightsByLeaguesId {
          flightCommentsByFlightsId {
            userByUsersId {
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
    if (response.data.favoriteRoutes[0].routeByRoutesId.leagueByLeaguesId.flightsByLeaguesId[0].flightCommentsByFlightsId[0].userByUsersId.email === 'osxcode@gmail.com') {
      console.log('✔︎ Test passed');
      process.exit();
    } else {
      console.log('✖ Test failed. Unexpected response.');
      console.log(response.data);
    }
  });
};

verifyDataImport();
