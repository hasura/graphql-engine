import React, { ChangeEvent, useReducer, useCallback, Dispatch } from 'react';

import {
  composeEndpoint,
  getCurrentPageHost,
  getStatusFromMessage,
  makeAPICall,
  parseEndpoints,
  parseQueryVariables,
  VariableData,
  supportedNumericTypes,
} from '../utils';
import {
  HeaderStateAction,
  headerStateReducer,
  requestLoadingStateReducer,
  variableStateReducer,
} from './reducer';
import Button from '../../../../Common/Button';
import { defaultLoadingState, HeaderState, VariableState } from './state';
import RequestHeadersContainer from './Headers';
import RequestVariablesContainer from './Variables';
import {
  AllowedRESTMethods,
  RestEndpointEntry,
} from '../../../../../metadata/types';
import Spinner from '../../../../Common/Spinner/Spinner';
import CollapsibleToggle from './CollapsibleToggle';

import styles from '../RESTStyles.scss';

interface EndpointState extends RestEndpointEntry {
  currentQuery: string;
}

export type DataHeader = {
  key: string;
  value: string;
  isActive: boolean;
};

type LivePreviewProps = {
  endpointState: EndpointState;
  pageHost: string;
  dataHeaders: DataHeader[];
};

const createInitialHeaderState = (headers?: DataHeader[]) => {
  if (!headers) {
    return [];
  }

  return headers
    .filter(header => header.key.trim() !== '' || header.value.trim() !== '')
    .map((header, index) => ({ ...header, isActive: true, index }));
};

const createInitialVariableState = (parsedVariables?: VariableData[]) => {
  if (!parsedVariables) {
    return [];
  }

  return parsedVariables.map(variableData => ({ ...variableData, value: '' }));
};

const collectHeaders = (allHeaders: HeaderState[]) =>
  allHeaders.reduce((acc, header) => {
    if (!header.isActive) {
      return acc;
    }

    return {
      ...acc,
      [header.key]: header.value,
    };
  }, {});

const getValueWithType = (variableData: VariableState) => {
  if (variableData.type === 'Boolean') {
    if (variableData.value.trim().toLowerCase() === 'false') {
      return false;
    }
    // NOTE: since everything that's not empty is considered as truthy
    return true;
  }

  if (supportedNumericTypes.includes(variableData.type)) {
    return Number(variableData.value);
  }

  return variableData.value?.trim()?.toString();
};

const composeRequestBody = (allVariables: VariableState[]) => {
  const vars = allVariables.reduce(
    (acc, variableData) => ({
      ...acc,
      [variableData.name]: getValueWithType(variableData),
    }),
    {}
  );

  return JSON.stringify(vars);
};

// in the spirit of DRY
const updateHeaderTextValues = (
  type: HeaderStateAction['type'],
  dispatch: Dispatch<HeaderStateAction>
) => (index: number) => (e: ChangeEvent<HTMLInputElement>) => {
  if (
    type !== 'RequestHeaders/SET_HEADER_KEY_TEXT' &&
    type !== 'RequestHeaders/SET_HEADER_VALUE_TEXT'
  ) {
    return;
  }

  dispatch({
    type,
    index,
    data: e.target.value,
  });
};

const getRequestMethod = (supportedMethods: AllowedRESTMethods[]) => {
  const filteredMethods = supportedMethods.filter(method => method !== 'GET');
  if (!filteredMethods || !filteredMethods.length) {
    return 'GET';
  }
  return filteredMethods[0];
};

const LivePreview: React.FC<LivePreviewProps> = ({
  pageHost,
  endpointState,
  dataHeaders,
}) => {
  const initialHeaderState = createInitialHeaderState(dataHeaders);
  const parsedQueryVariables = parseQueryVariables(endpointState.currentQuery);
  const initialVariableState = createInitialVariableState(parsedQueryVariables);

  const [headerState, headerDispatch] = useReducer(
    headerStateReducer,
    initialHeaderState
  );
  const [variableState, variableDispatch] = useReducer(
    variableStateReducer,
    initialVariableState
  );
  const [progressState, progressDispatch] = useReducer(
    requestLoadingStateReducer,
    defaultLoadingState
  );

  const updateHeaderKeyText = updateHeaderTextValues(
    'RequestHeaders/SET_HEADER_KEY_TEXT',
    headerDispatch
  );
  const updateHeaderValueText = updateHeaderTextValues(
    'RequestHeaders/SET_HEADER_VALUE_TEXT',
    headerDispatch
  );

  const updateActiveStateForHeader = (index: number) => () => {
    const checkIsHeaderPresent = headerState.find(
      header => header.index === index
    );

    if (!checkIsHeaderPresent) {
      return;
    }

    headerDispatch({
      type: 'RequestHeaders/TOGGLE_ACTIVE_STATE_OF_HEADER',
      data: !checkIsHeaderPresent.isActive,
      index,
    });
  };

  const updateVariableText = (name: string) => (
    e: ChangeEvent<HTMLInputElement>
  ) => {
    variableDispatch({
      type: 'RequestVariables/SET_HEADER_VALUE_TEXT',
      data: e.target.value,
      name,
    });
  };

  const onClickAddHeader = () => {
    headerDispatch({
      type: 'RequestHeaders/ADD_NEW_EMPTY_HEADER',
    });
  };

  const onClickRemoveHeader = (index: number) => () => {
    headerDispatch({
      type: 'RequestHeaders/REMOVE_HEADER_FROM_LIST',
      index,
    });
  };

  const runQuery = useCallback(() => {
    let requestURL = pageHost;
    const requestHeaders = collectHeaders(headerState);
    const requestMethod = getRequestMethod(endpointState.methods);
    let body;

    const parsedEndpoint = parseEndpoints(endpointState.url);
    if (!parsedEndpoint) {
      // should never happen, because empty endpoints are not allowed
      return;
    }

    const urlQueryVariables = parsedEndpoint.filter(
      path => path.type === 'variable'
    );
    if (urlQueryVariables.length || variableState.length) {
      // TODO: check this conditional
      if (requestMethod !== 'GET' && !urlQueryVariables.length) {
        body = composeRequestBody(variableState);
      } else {
        const currentPageHost = getCurrentPageHost();
        requestURL = `${currentPageHost}/api/rest/${composeEndpoint(
          parsedEndpoint,
          variableState,
          endpointState.url
        )}`;
        body = undefined;
      }
    }

    progressDispatch({
      type: 'RequestLoadingState/SET_LOADING_STATE',
    });
    makeAPICall({
      url: requestURL,
      headers: requestHeaders,
      method: requestMethod,
      body,
    })
      .then(data => {
        progressDispatch({
          type: 'RequestLoadingState/SET_REQUEST_SUCCESS',
          data,
        });
      })
      .catch(err => {
        progressDispatch({
          type: 'RequestLoadingState/SET_REQUEST_ERRORED',
          data: err,
        });
      });
  }, [
    endpointState.methods,
    endpointState.url,
    headerState,
    variableState,
    pageHost,
  ]);

  return (
    <div className={styles.rest_live_layout}>
      <h3 className={styles.rest_live_header}>Preview Request</h3>
      <div className={styles.rest_preview_req_header_layout}>
        <CollapsibleToggle
          title="Request Headers"
          state={headerState}
          properties={['key', 'value']}
        >
          <RequestHeadersContainer
            headerState={headerState}
            onClickRemove={onClickRemoveHeader}
            updateKeyText={updateHeaderKeyText}
            updateValueText={updateHeaderValueText}
            toggleActiveState={updateActiveStateForHeader}
          />
          <Button
            size="sm"
            onClick={onClickAddHeader}
            className={styles.float_right}
          >
            <i className={`fa fa-plus-circle ${styles.icon_margin}`} />
            Add Header
          </Button>
        </CollapsibleToggle>
      </div>
      <div className={styles.rest_preview_req_var_layout}>
        <CollapsibleToggle
          title="Request Variables"
          state={variableState}
          properties={['name', 'value']}
        >
          <RequestVariablesContainer
            variablesState={variableState}
            updateVariableValue={updateVariableText}
          />
        </CollapsibleToggle>
      </div>
      <div className={styles.rest_preview_req_var_layout}>
        <hr className="my-md" />
        <Button
          size="sm"
          onClick={runQuery}
          color="yellow"
          className={styles.float_right}
        >
          <i className={`fa fa-play ${styles.icon_margin}`} />
          Run Request
        </Button>
      </div>
      <div className={styles.rest_preview_show_response}>
        {progressState.isLoading ? <Spinner /> : null}
        {progressState?.data && (
          <pre className={styles.live_preview_pre}>
            {JSON.stringify(progressState.data, null, 4)}
          </pre>
        )}
        {progressState?.error && (
          <div className={styles.rest_preview_error_display}>
            <b>Error Message: </b> {progressState?.error?.message}
            <div>
              <b>Error Status code: </b>
              {getStatusFromMessage(progressState?.error?.message) ??
                'Request Status Unknown'}
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

export default LivePreview;
