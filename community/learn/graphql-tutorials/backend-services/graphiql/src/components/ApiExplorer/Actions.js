import defaultState from './state';
// import fetch from 'isomorphic-fetch';

import { SubscriptionClient } from 'subscriptions-transport-ws';
import { WebSocketLink } from 'apollo-link-ws';
import { parse } from 'graphql';
import { execute } from 'apollo-link';
import { push } from 'react-router-redux';

const REQUEST_HEADER_CHANGED = 'ApiExplorer/REQUEST_HEADER_CHANGED';
const REQUEST_HEADER_ADDED = 'ApiExplorer/REQUEST_HEADER_ADDED';
const REQUEST_HEADER_REMOVED = 'ApiExplorer/REQUEST_HEADER_REMOVED';

const FOCUS_ROLE_HEADER = 'ApiExplorer/FOCUS_ROLE_HEADER';
const UNFOCUS_ROLE_HEADER = 'ApiExplorer/UNFOCUS_ROLE_HEADER';
const GRAPHQL_ENDPOINT_CHANGED = 'ApiExplorer/GRAPHQL_ENDPOINT_CHANGED';

import { getHeadersAsJSON } from './utils';

const focusHeaderTextbox = () => ({ type: FOCUS_ROLE_HEADER });
const unfocusTypingHeader = () => ({ type: UNFOCUS_ROLE_HEADER });

const updateGraphQLEndpoint = (endpoint) => {
  return (dispatch) => {
    dispatch({ type: GRAPHQL_ENDPOINT_CHANGED, data: endpoint });
    // set local storage
    window.localStorage.setItem('ONLINE_GRAPHIQL_ENDPOINT', endpoint);
    dispatch(push('/graphiql'));
  };
};

const getRemoteQueries = (queryUrl, cb) => {
  fetch(queryUrl)
    .then(resp => resp.text().then(cb))
    .catch(e => console.log('Invalid query URL: ', e));
};

const createWsClient = (url, headers) => {
  const gqlUrl = new URL(url);
  let websocketProtocol = 'ws';
  if (gqlUrl.protocol === 'https:') {
    websocketProtocol = 'wss';
  }
  const headersFinal = getHeadersAsJSON(headers);
  const graphqlUrl = `${websocketProtocol}://${url.split('//')[1]}`;
  const client = new SubscriptionClient(graphqlUrl, {
    connectionParams: {
      headers: {
        ...headersFinal,
      },
    },
    reconnect: true,
  });
  return client;
};

const graphqlSubscriber = (graphQLParams, url, headers) => {
  const link = new WebSocketLink(createWsClient(url, headers));
  try {
    const fetcher = operation => {
      operation.query = parse(operation.query);
      return execute(link, operation);
    };
    return fetcher(graphQLParams);
  } catch (e) {
    return e.json();
  }
};

const isSubscription = graphQlParams => {
  const queryDoc = parse(graphQlParams.query);
  for (const definition of queryDoc.definitions) {
    if (definition.kind === 'OperationDefinition') {
      const operation = definition.operation;
      if (operation === 'subscription') {
        return true;
      }
    }
  }
  return false;
};

const graphQLFetcherFinal = (graphQLParams, url, headers) => {
  if (isSubscription(graphQLParams)) {
    return graphqlSubscriber(graphQLParams, url, headers);
  }
  return fetch(url, {
    method: 'POST',
    headers: getHeadersAsJSON(headers),
    body: JSON.stringify(graphQLParams),
  }).then(response => response.json());
};

const changeRequestHeader = (index, key, newValue, isDisabled) => ({
  type: REQUEST_HEADER_CHANGED,
  data: {
    index: index,
    keyName: key,
    newValue: newValue,
    isDisabled: isDisabled,
  },
});

const addRequestHeader = (key, value) => ({
  type: REQUEST_HEADER_ADDED,
  data: {
    key: key,
    value: value,
  },
});

const removeRequestHeader = index => {
  return {
    type: REQUEST_HEADER_REMOVED,
    data: index,
  };
};

// This method adds the new header and moves the empty header to the bottom of the list
const getHeadersAfterAddingNewHeader = (headers, newHeader) => {
  const nonEmptyHeaders = headers.filter(header => {
    return !header.isNewHeader;
  });
  nonEmptyHeaders.push(newHeader);
  nonEmptyHeaders.push({
    key: '',
    value: '',
    isActive: false,
    isNewHeader: true,
  });
  return nonEmptyHeaders;
};

// This method adds a new empty header if no empty header is present
const getChangedHeaders = (headers, changedHeaderDetails) => {
  const newHeaders = Object.assign([], headers);
  if (newHeaders[changedHeaderDetails.index].isNewHeader) {
    newHeaders[changedHeaderDetails.index].isNewHeader = false;
    newHeaders[changedHeaderDetails.index].isActive = true;
    newHeaders[changedHeaderDetails.index].isDisabled = false;
  }
  if (changedHeaderDetails.keyName === 'isActive') {
    newHeaders[changedHeaderDetails.index].isActive = !newHeaders[
      changedHeaderDetails.index
    ].isActive;
  } else {
    newHeaders[changedHeaderDetails.index][changedHeaderDetails.keyName] =
      changedHeaderDetails.newValue;
  }
  if (changedHeaderDetails.isDisabled === true) {
    newHeaders[changedHeaderDetails.index].isDisabled = true;
  } else {
    newHeaders[changedHeaderDetails.index].isDisabled = false;
  }
  const nonEmptyHeaders = newHeaders.filter(header => {
    return !header.isNewHeader;
  });
  nonEmptyHeaders.push({
    key: '',
    value: '',
    isActive: false,
    isNewHeader: true,
    isDisabled: false,
  });
  return nonEmptyHeaders;
};

const apiExplorerReducer = (state = defaultState, action) => {
  switch (action.type) {
    case REQUEST_HEADER_CHANGED:
      return {
        ...state,
        displayedApi: {
          ...state.displayedApi,
          request: {
            ...state.displayedApi.request,
            headers: getChangedHeaders(
              state.displayedApi.request.headers,
              action.data
            ),
          },
        },
      };
    case REQUEST_HEADER_ADDED:
      return {
        ...state,
        displayedApi: {
          ...state.displayedApi,
          request: {
            ...state.displayedApi.request,
            headers: getHeadersAfterAddingNewHeader(
              state.displayedApi.request.headers,
              {
                key: action.data.key,
                value: action.data.value,
                isActive: true,
                isNewHeader: false,
              }
            ),
          },
        },
      };
    case REQUEST_HEADER_REMOVED:
      return {
        ...state,
        displayedApi: {
          ...state.displayedApi,
          request: {
            ...state.displayedApi.request,
            headers: state.displayedApi.request.headers.filter((header, i) => {
              return !(i === action.data);
            }),
          },
        },
      };
    case UNFOCUS_ROLE_HEADER:
      return {
        ...state,
        headerFocus: false,
      };
    case FOCUS_ROLE_HEADER:
      return {
        ...state,
        headerFocus: true,
      };
    case GRAPHQL_ENDPOINT_CHANGED:
      return {
        ...state,
        graphqlEndpoint: action.data,
      };
    default:
      return state;
  }
};

export default apiExplorerReducer;

export {
  changeRequestHeader,
  addRequestHeader,
  removeRequestHeader,
  graphQLFetcherFinal,
  focusHeaderTextbox,
  unfocusTypingHeader,
  getRemoteQueries,
  updateGraphQLEndpoint,
};
