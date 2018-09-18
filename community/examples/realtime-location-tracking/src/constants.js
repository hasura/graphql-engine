// Constants file

const GOOGLE_API_KEY = process.env.NODE_ENV==='production'? process.env.REACT_APP_GOOGLE_API_KEY : ''; 

const HASURA_GRAPHQL_URL = 'realtime-backend.herokuapp.com/v1alpha1/graphql';

const wsurl = `wss://${ HASURA_GRAPHQL_URL }`;
const httpurl = `https://${ HASURA_GRAPHQL_URL }`;

const HASURA_LOCATION = {
  lat: 12.93958,
  lng: 77.62047,
  // lat: 12.939553,
  // lng: 77.620519
};

const bounds =  {
  "ne" : {
    "lat" : 12.940464,
    "lng" : 77.6207663
  },
  "sw" : {
    "lat" : 12.929445,
    "lng" : 77.60969620000002
  }
};

export {
  GOOGLE_API_KEY,
  HASURA_LOCATION,
  bounds,
  wsurl,
  httpurl,
};
