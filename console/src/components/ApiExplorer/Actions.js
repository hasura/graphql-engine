import defaultState from './state';
// import fetch from 'isomorphic-fetch';

import { SubscriptionClient } from 'subscriptions-transport-ws';
import { WebSocketLink } from 'apollo-link-ws';
import { parse } from 'graphql';
import { execute } from 'apollo-link';

import { getHeadersAsJSON } from './utils';
import { saveAppState, clearState } from '../AppState.js';

const CHANGE_TAB = 'ApiExplorer/CHANGE_TAB';
const CHANGE_API_SELECTION = 'ApiExplorer/CHANGE_API_SELECTION';
const EXPAND_AUTH_API = 'ApiExplorer/EXPAND_AUTH_API';
const CODE_GENERATOR_OPEN = 'ApiExplorer/CODE_GENERATOR_OPEN';
const CODE_GENERATOR_CLOSE = 'ApiExplorer/CODE_GENERATOR_CLOSE';
const CODE_GENERATOR_CHANGE_SELECTION =
  'ApiExplorer/CODE_GENERATOR_CHANGE_SELECTION';
const CODE_GENERATOR_COPY_TO_CLIPBOARD =
  'ApiExplorer/CODE_GENERATOR_COPY_TO_CLIPBOARD';

const REQUEST_METHOD_CHANGED = 'ApiExplorer/REQUEST_METHOD_CHANGED';
const REQUEST_URL_CHANGED = 'ApiExplorer/REQUEST_URL_CHANGED';
const REQUEST_PARAMS_CHANGED = 'ApiExplorer/REQUEST_PARAMS_CHANGED';
const REQUEST_HEADER_CHANGED = 'ApiExplorer/REQUEST_HEADER_CHANGED';
const REQUEST_HEADER_ADDED = 'ApiExplorer/REQUEST_HEADER_ADDED';
const REQUEST_HEADER_REMOVED = 'ApiExplorer/REQUEST_HEADER_REMOVED';

const MAKING_API_REQUEST = 'ApiExplorer/MAKING_API_REQUEST';
const RESET_MAKING_REQUEST = 'ApiExplorer/RESET_MAKING_REQUEST';
const API_REQUEST_SUCCESS = 'ApiExplorer/API_REQUEST_SUCCESS';
const API_REQUEST_FAILURE = 'ApiExplorer/API_REQUEST_FAILURE';

const CLEAR_HISTORY = 'ApiExplorer/CLEAR_HISTORY';
const UPDATE_FILE_OBJECT = 'ApiExplorer/UPDATE_FILE_OBJECT';

const CREATE_WEBSOCKET_CLIENT = 'ApiExplorer/CREATE_WEBSOCKET_CLIENT';

const FOCUS_ROLE_HEADER = 'ApiExplorer/FOCUS_ROLE_HEADER';
const UNFOCUS_ROLE_HEADER = 'ApiExplorer/UNFOCUS_ROLE_HEADER';

const clearHistory = () => {
  return {
    type: CLEAR_HISTORY,
  };
};

const updateFileObject = fileObj => {
  return { type: UPDATE_FILE_OBJECT, data: fileObj };
};

const focusHeaderTextbox = () => ({ type: FOCUS_ROLE_HEADER });
const unfocusTypingHeader = () => ({ type: UNFOCUS_ROLE_HEADER });

const copyCodeToClipboard = isCopying => {
  return {
    type: CODE_GENERATOR_COPY_TO_CLIPBOARD,
    data: isCopying,
  };
};

const changeCodeGeneratorSelection = newSelection => {
  return {
    type: CODE_GENERATOR_CHANGE_SELECTION,
    data: newSelection,
  };
};

const changeRequestMethod = newMethod => {
  return {
    type: REQUEST_METHOD_CHANGED,
    data: newMethod,
  };
};

const changeRequestUrl = newUrl => {
  return {
    type: REQUEST_URL_CHANGED,
    data: newUrl,
  };
};

const changeRequestParams = newParams => {
  return dispatch => {
    dispatch({ type: REQUEST_PARAMS_CHANGED, data: newParams });
  };
};

const createWsClient = (url, headers) => {
  const gqlUrl = new URL(url);
  const windowUrl = new URL(window.location);
  let websocketProtocol = 'ws';
  if (gqlUrl.protocol === 'https:' && windowUrl.protocol === 'https:') {
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

/* Analyse Fetcher */
const analyzeFetcher = (url, headers, analyzeApiChange) => {
  return query => {
    const editedQuery = {
      query,
    };
    let user = {};
    const reqHeaders = getHeadersAsJSON(headers);
    if (!analyzeApiChange) {
      user.role = 'admin';
      user.headers = reqHeaders;
    } else {
      user = {
        'x-hasura-role': 'admin',
      };
    }

    // Check if x-hasura-role is available in some form in the headers
    const totalHeaders = Object.keys(reqHeaders);
    totalHeaders.forEach(t => {
      // If header has x-hasura-*
      const lHead = t.toLowerCase();
      if (
        lHead.slice(0, 'x-hasura-'.length) === 'x-hasura-' &&
        lHead !== 'x-hasura-access-key' &&
        lHead !== 'x-hasura-admin-secret'
      ) {
        user[lHead] = reqHeaders[t];
        delete reqHeaders[t];
      }
    });

    editedQuery.user = user;
    return fetch(`${url}/explain`, {
      method: 'post',
      headers: reqHeaders,
      body: JSON.stringify(editedQuery),
      credentials: 'include',
    });
  };
};
/* End of it */

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

const generateApiCodeClicked = () => {
  return {
    type: CODE_GENERATOR_OPEN,
  };
};

const closeCodeGeneratorClicked = () => {
  return {
    type: CODE_GENERATOR_CLOSE,
  };
};

const changeTabSelection = newSelectionIndex => {
  return {
    type: CHANGE_TAB,
    data: newSelectionIndex,
  };
};

const changeApiSelection = (newSelectedApi, index) => {
  return {
    type: CHANGE_API_SELECTION,
    data: newSelectedApi,
    index: index,
  };
};

const editGeneratedJson = () => {
  return (dispatch, getState) => {
    const tabs = getState().apiexplorer.tabs;
    if (
      tabs[0] &&
      tabs[0].content &&
      tabs[0].content[0] &&
      tabs[0].content[0].content[1]
    ) {
      const newSelectedApi = tabs[0].content[0].content[2];
      const existingJson = getState().apiexplorer.displayedApi.request.params;
      newSelectedApi.request.params = existingJson;
      dispatch(changeApiSelection(newSelectedApi, 'Username-password Login'));
    }
  };
};

const expandAuthApi = index => {
  return {
    type: EXPAND_AUTH_API,
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

const getStateAfterAddingRequestToHistory = oldState => {
  const newState = Object.assign({}, oldState);
  // Check if history is present
  const isHistoryPresent = newState.tabs[1].content.length > 0;
  if (!isHistoryPresent) {
    newState.tabs[1].content.push(
      {
        title: 'Clear History',
        content: [],
      },
      {
        title: '',
        content: [],
      }
    );
  }
  const index = newState.tabs[1].content[1].content.length;
  const newRequest = {
    id: 'History-' + index,
    details: {
      title: newState.displayedApi.details.title,
      description: '',
    },
    request: newState.displayedApi.request,
  };
  newState.tabs[1].content[1].content.unshift(newRequest);

  // Saving the state to the local storage
  saveAppState(newState.tabs[1]);

  return newState;
};

const getStateAfterClearingHistory = state => {
  clearState();
  return {
    ...state,
    tabs: [
      state.tabs[0],
      {
        ...state.tabs[1],
        content: [],
      },
    ],
  };
};

const getRemoteQueries = (queryUrl, cb) => {
  fetch(queryUrl)
    .then(resp => resp.text().then(cb))
    .catch(e => console.log('Invalid query file URL: ', e));
};

const apiExplorerReducer = (state = defaultState, action) => {
  switch (action.type) {
    case CHANGE_TAB:
      return {
        ...state,
        currentTab: action.data,
      };
    case CHANGE_API_SELECTION:
      return {
        ...state,
        displayedApi: action.data,
        explorerData: {
          ...state.explorerData,
          response: {},
        },
      };
    case EXPAND_AUTH_API:
      return {
        ...state,
        authApiExpanded: action.data,
      };
    case CODE_GENERATOR_OPEN:
      return {
        ...state,
        modalState: {
          ...state.modalState,
          isOpen: true,
        },
      };
    case CODE_GENERATOR_CLOSE:
      return {
        ...state,
        modalState: {
          ...state.modalState,
          isOpen: false,
        },
      };
    case CODE_GENERATOR_CHANGE_SELECTION:
      return {
        ...state,
        modalState: {
          ...state.modalState,
          selectedCodeGen: action.data,
        },
      };
    case CODE_GENERATOR_COPY_TO_CLIPBOARD:
      return {
        ...state,
        modalState: {
          ...state.modalState,
          isCopied: action.data,
        },
      };
    case REQUEST_METHOD_CHANGED:
      return {
        ...state,
        displayedApi: {
          ...state.displayedApi,
          request: {
            ...state.displayedApi.request,
            method: action.data,
          },
        },
      };
    case REQUEST_URL_CHANGED:
      return {
        ...state,
        displayedApi: {
          ...state.displayedApi,
          request: {
            ...state.displayedApi.request,
            url: action.data,
          },
        },
      };
    case REQUEST_PARAMS_CHANGED:
      return {
        ...state,
        displayedApi: {
          ...state.displayedApi,
          request: {
            ...state.displayedApi.request,
            params: action.data,
          },
        },
      };
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
    case MAKING_API_REQUEST:
      return {
        ...state,
        explorerData: {
          ...state.explorerData,
          sendingRequest: true,
          enableResponseSection: false,
          response: {},
        },
      };
    case API_REQUEST_SUCCESS:
      const newState = getStateAfterAddingRequestToHistory(state);
      return {
        ...newState,
        explorerData: {
          ...newState.explorerData,
          sendingRequest: false,
          enableResponseSection: true,
          response: action.data,
        },
      };
    case API_REQUEST_FAILURE:
      const newState2 = getStateAfterAddingRequestToHistory(state);
      return {
        ...newState2,
        explorerData: {
          ...newState2.explorerData,
          sendingRequest: false,
          enableResponseSection: true,
          response: action.data,
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
    case CLEAR_HISTORY:
      return { ...state, ...getStateAfterClearingHistory(state) };
    case UPDATE_FILE_OBJECT:
      return {
        ...state,
        explorerData: {
          ...state.explorerData,
          fileObj: action.data,
        },
      };
    case RESET_MAKING_REQUEST:
      return {
        ...state,
        explorerData: {
          ...state.explorerData,
          sendingRequest: false,
          enableResponseSection: false,
        },
      };
    case CREATE_WEBSOCKET_CLIENT:
      return {
        ...state,
        webSocketClient: action.data,
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
    default:
      return state;
  }
};

export default apiExplorerReducer;

export {
  changeTabSelection,
  changeApiSelection,
  expandAuthApi,
  generateApiCodeClicked,
  closeCodeGeneratorClicked,
  changeCodeGeneratorSelection,
  copyCodeToClipboard,
  changeRequestMethod,
  changeRequestUrl,
  changeRequestParams,
  changeRequestHeader,
  addRequestHeader,
  removeRequestHeader,
  clearHistory,
  updateFileObject,
  editGeneratedJson,
  graphQLFetcherFinal,
  createWsClient,
  focusHeaderTextbox,
  unfocusTypingHeader,
  getRemoteQueries,
  analyzeFetcher,
};
