// Constants file

const GOOGLE_API_KEY = process.env.NODE_ENV==='production'? process.env.REACT_APP_GOOGLE_API_KEY : ''; 

const HASURA_GRAPHQL_ENGINE_HOSTNAME = 'realtime-location-tracking.demo.hasura.app';

const scheme = (proto) => {
  return window.location.protocol === 'https:' ? `${proto}s` : proto;
}

const wsurl = `${scheme('ws')}://${HASURA_GRAPHQL_ENGINE_HOSTNAME}/v1/graphql`;
const httpurl = `${scheme('http')}://${HASURA_GRAPHQL_ENGINE_HOSTNAME}/v1/graphql`;

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
