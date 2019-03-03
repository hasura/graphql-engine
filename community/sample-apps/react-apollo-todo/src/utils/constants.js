// const HASURA_GRAPHQL_ENGINE_HOSTNAME = window.location.host;
const HASURA_GRAPHQL_ENGINE_HOSTNAME = "react-apollo-todo.demo.hasura.app";

const scheme = proto => {
  // return window.location.protocol === 'https:' ? `${proto}s` : proto;
  return proto;
};

export const GRAPHQL_URL = `${scheme(
  "https"
)}://${HASURA_GRAPHQL_ENGINE_HOSTNAME}/v1alpha1/graphql`;
export const REALTIME_GRAPHQL_URL = `${scheme(
  "wss"
)}://${HASURA_GRAPHQL_ENGINE_HOSTNAME}/v1alpha1/graphql`;
export const authClientId = "Fl-hdc6xdYIkok9ynbcL6zoUZPAIdOZN";
export const authDomain = "hasura-react-apollo-todo.auth0.com";
// export const callbackUrl = `${scheme('http')}://${HASURA_GRAPHQL_ENGINE_HOSTNAME}/callback`;
export const callbackUrl = "http://localhost:3000/callback";
