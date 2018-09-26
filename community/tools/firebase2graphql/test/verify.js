const {query} = require('graphqurl');

const complexQuery = `
query {
  Album (
    order_by:_id_asc
  ){
    _id
    Album_artist {
      Name
      ArtistId
    }
    Album_tracks (
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
      response.data.Album[0].Album_artist.ArtistId === 1 &&
      response.data.Album[0].Album_tracks[0].Name === 'Breaking The Rules'
    ) {
      console.log('✔︎ Test passed');
      process.exit();
    } else {
      console.log('✖ Test failed. Unexpected response.');
      console.log(response.data);
    }
  });
};

verifyDataImport();
