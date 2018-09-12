const fetch = require('node-fetch');
const { query } = require('graphqurl');

const MUTATION_UPDATE_LAT_LONG = `
mutation updateLatLong($id: Int!, $lat: numeric! $lng: numeric!) {
  update_profile(
    where: {id: {_eq: $id}},
    _set: {
      lat: $lat,
      lng: $lng
    }
  ) {
    affected_rows
    returning {
      id
      lat
      lng
    }
  }
}`;

exports.trigger = async (req, res) => {
  const GMAPS_API_KEY = process.env.GMAPS_API_KEY;
  const HASURA_GRAPHQL_ENGINE_URL = process.env.HASURA_GRAPHQL_ENGINE_URL;

  const { event: {op, data}, table } = req.body;

  if (op === 'INSERT' && table.name === 'profile' && table.schema === 'public') {
    const { id, address } = data.new;
    const gmaps = await fetch(`https://maps.googleapis.com/maps/api/geocode/json?address=${address}&key=${GMAPS_API_KEY}`);
    const gmapsResponse = await gmaps.json();
    if (gmapsResponse.results && gmapsResponse.results.length === 0) {
      res.json({error: true, data: gmapsResponse, message: `no results for address ${address}`});
      return;
    }
    const { lat, lng } = gmapsResponse.results[0].geometry.location;
    const hgeResponse = await query({
      query: MUTATION_UPDATE_LAT_LONG,
      endpoint: HASURA_GRAPHQL_ENGINE_URL,
      variables: { id, lat, lng },
    });
    res.json({error: false, data: hgeResponse.data});
  } else {
    res.json({error: false, message: 'ignored event'});
  }
};
