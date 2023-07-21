import React, { ChangeEvent, useReducer, useCallback, Dispatch } from 'react';
import { FaPlay, FaPlusCircle } from 'react-icons/fa';

import { Button } from '../../../../../new-components/Button';
import {
  composeEndpoint,
  getCurrentPageHost,
  getStatusFromMessage,
  makeAPICall,
  parseEndpoints,
  parseQueryVariables,
  VariableData,
  getRequestBody,
} from '../utils';
import {
  HeaderStateAction,
  headerStateReducer,
  requestLoadingStateReducer,
  variableStateReducer,
} from './reducer';
import { defaultLoadingState, HeaderState } from './state';
import RequestHeadersContainer from './Headers';
import RequestVariablesContainer from './Variables';
import {
  AllowedRESTMethods,
  RestEndpointEntry,
} from '../../../../../metadata/types';
import Spinner from '../../../../Common/Spinner/Spinner';
import CollapsibleToggle from './CollapsibleToggle';

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

// in the spirit of DRY
const updateHeaderTextValues =
  (type: HeaderStateAction['type'], dispatch: Dispatch<HeaderStateAction>) =>
  (index: number) =>
  (e: ChangeEvent<HTMLInputElement>) => {
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

  const updateVariableText =
    (name: string) => (e: ChangeEvent<HTMLInputElement>) => {
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
    const requestHeaders = collectHeaders(headerState);
    const requestMethod = getRequestMethod(endpointState.methods);

    const parsedEndpoint = parseEndpoints(endpointState.url);
    if (!parsedEndpoint) {
      // should never happen, because empty endpoints are not allowed
      return;
    }
    const urlQueryVariables = parsedEndpoint.filter(
      path => path.type === 'variable'
    );

    const body = getRequestBody({
      urlQueryVariables,
      variableState,
    });

    const requestURL = urlQueryVariables.length
      ? `${getCurrentPageHost()}/api/rest/${composeEndpoint(
          parsedEndpoint,
          variableState,
          endpointState.url
        )}`
      : pageHost;

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
    <div className="flex flex-col pb-md text-center">
      <h3 className="text-lg font-bold">Preview Request</h3>
      <div className="my-md w-full">
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
            icon={<FaPlusCircle />}
            iconPosition="start"
            onClick={onClickAddHeader}
            className="float-right"
          >
            Add Header
          </Button>
        </CollapsibleToggle>
      </div>
      <div className="w-full">
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
      <div className="mb-sm w-full">
        <hr className="my-md" />
        <Button
          size="md"
          mode="primary"
          onClick={runQuery}
          className="float-right"
        >
          <FaPlay className="mr-xs" />
          Run Request
        </Button>
      </div>
      <div className="w-full">
        {progressState.isLoading ? <Spinner /> : null}
        {progressState?.data && (
          <pre className="h-[200] overflow-scroll text-left">
            {JSON.stringify(progressState.data, null, 4)}
          </pre>
        )}
        {progressState?.error && (
          <div className="w-full border border-gray-500 rounded-sm p-sm pt-md">
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
