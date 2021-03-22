import React, { useReducer, useEffect, useRef } from 'react';
import { RouteComponentProps } from 'react-router';
import { connect, ConnectedProps } from 'react-redux';

import {
  AllowedRESTMethods,
  RestEndpointEntry,
} from '../../../../../metadata/types';
import { Dispatch, ReduxState } from '../../../../../types';
import { getLSItem, LS_KEYS } from '../../../../../utils/localStorage';
import MethodsInput from './MethodsInput';
import createEndpointReducer from './reducer';
import { CreateEndpointState, defaultState } from './state';
import Input from './Input';
import BreadCrumb from '../../../../Common/Layout/BreadCrumb/BreadCrumb';
import Button from '../../../../Common/Button';
import RequestViewer from './RequestViewer';
import _push from '../../../Data/push';
import {
  addRESTEndpoint,
  editRESTEndpoint,
} from '../../../../../metadata/actions';
import { allowedQueriesCollection } from '../../../../../metadata/utils';
import { getCurrentPageHost, isQueryValid } from '../utils';
import { showErrorNotification } from '../../../Common/Notification';
import URLPreview from './URLPreview';

import styles from '../RESTStyles.scss';

const locationNote = `This is the location of your endpoint (must be unique).\n
Any parameterized variables (eg. ${getCurrentPageHost()}/api/rest/example/:id will be made available to your request.`;

// cleanUpState is used to trim the string fields used in the create/edit form
const cleanUpState = (state: CreateEndpointState) => {
  const modifiedName = state.name.trim();
  const modifiedURL = state.url.trim();
  const modifiedComment = state.comment.trim();
  const modifedRequest = state.request.trim();

  return {
    ...state,
    name: modifiedName,
    url: modifiedURL,
    comment: modifiedComment,
    request: modifedRequest,
  };
};

const isErroredState = (state: CreateEndpointState) => {
  const errorFields = [];
  if (!state.name) {
    errorFields.push('Name');
  }
  if (!state.url) {
    errorFields.push('Location');
  }
  if (!state.request) {
    errorFields.push('Request');
  }
  if (!state.methods.length) {
    errorFields.push('Method');
  }

  return errorFields;
};

const createEndpointObject = (
  state: CreateEndpointState
): [RestEndpointEntry, string] => [
  {
    name: state.name,
    url: state.url,
    definition: {
      query: {
        query_name: state.name,
        collection_name: allowedQueriesCollection,
      },
    },
    methods: state.methods,
    comment: state.comment || undefined,
  },
  state.request,
];

interface CreateEndpointProps extends InjectedProps {
  location: RouteComponentProps<unknown, unknown>['location'];
  routeParams: RouteComponentProps<unknown, { name: string }>['routeParams'];
}

// This component will be used for the purposes of edit as well
const CreateEndpoint: React.FC<CreateEndpointProps> = ({
  routeToPage,
  createEndpoint,
  location,
  metadataObject,
  editEndpoint,
  routeParams,
  showError,
}) => {
  const [inputState, inputDispatch] = useReducer(
    createEndpointReducer,
    defaultState
  );
  const oldState = useRef<RestEndpointEntry | null>(null);
  const isPageCreate = location.pathname === '/api/rest/create';
  // currentPageName will have a valid value, when it's on the edit page
  let currentPageName = '';
  if (!isPageCreate) {
    currentPageName = routeParams.name;
  }

  const crumbs = [
    { url: '/api/rest/list', title: 'REST Endpoints' },
    { url: '', title: `${isPageCreate ? 'Create' : 'Edit'} Endpoint` },
  ];

  const updateEndpointName = (name: string) => {
    inputDispatch({
      type: 'CreateEndpoint/UPDATE_ENDPOINT_NAME',
      data: name,
    });
  };

  const updateEndpointComment = (comment: string) => {
    inputDispatch({
      type: 'CreateEndpoint/UPDATE_ENDPOINT_COMMENT',
      data: comment,
    });
  };

  const updateEndpointURL = (url: string) => {
    inputDispatch({
      type: 'CreateEndpoint/UPDATE_ENDPOINT_URL',
      data: url,
    });
  };

  const updateEndpointRequest = (request: string) => {
    inputDispatch({
      type: 'CreateEndpoint/UPDATE_ENDPOINT_REQUEST',
      data: request,
    });
  };

  const updateEndpointMethods = (methods: AllowedRESTMethods[]) => {
    inputDispatch({
      type: 'CreateEndpoint/UPDATE_ENDPOINT_METHODS',
      data: methods,
    });
  };

  const resetInputState = () => {
    inputDispatch({
      type: 'CreateEndpoint/RESET_INPUT_STATE',
    });
  };

  const resetPageState = () => {
    resetInputState();
    routeToPage('/api/rest/list');
  };

  useEffect(() => {
    // onload, fetch the query from GraphiQL localstorage on create page
    if (isPageCreate) {
      const graphqlRequest = getLSItem(LS_KEYS.graphiqlQuery) ?? '';
      updateEndpointRequest(graphqlRequest);
      return;
    }
    const currentEndpoints = metadataObject?.rest_endpoints;
    const currentCollections = metadataObject?.query_collections;
    if (!currentEndpoints || !currentCollections) {
      // Ideally, this should not be the case
      return;
    }
    const oldRestEndpointEntry = currentEndpoints.find(
      et => et.name === currentPageName
    );
    if (!oldRestEndpointEntry) {
      // Ideally, this also, should not be happening
      return;
    }
    oldState.current = oldRestEndpointEntry;
    updateEndpointName(oldRestEndpointEntry.name);
    updateEndpointComment(oldRestEndpointEntry?.comment ?? '');
    updateEndpointURL(oldRestEndpointEntry.url);
    updateEndpointMethods(oldRestEndpointEntry.methods);
    // Get the actual query from the collection
    const allAllowedQueriesCollection = currentCollections.find(
      qce => qce.name === allowedQueriesCollection
    );
    if (!allAllowedQueriesCollection) {
      return;
    }
    const foundQuery = allAllowedQueriesCollection.definition.queries.find(
      qc => qc.name === currentPageName
    );
    if (!foundQuery?.query) {
      updateEndpointRequest('');
      return;
    }
    updateEndpointRequest(foundQuery.query);
  }, []);

  const onClickCreate = () => {
    const cleanedUpState = cleanUpState(inputState);
    const catchedError = isErroredState(cleanedUpState);
    const errorLen = catchedError.length;
    if (errorLen >= 1) {
      const fieldText = `field${errorLen >= 2 ? 's' : ''}`;
      const isAreText = `${errorLen >= 2 ? 'are' : 'is a'}`;
      showError(
        'Some required fields are empty',
        `${catchedError.join(', ')} ${isAreText} required ${fieldText}.`
      );
      return;
    }
    // NOTE: this check is necessary for the edit page
    // where queries can be edited
    if (!isQueryValid(cleanedUpState.request)) {
      showError(
        'Invalid Query being used to create endpoint',
        'Please note that only query and mutations are allowed while creating endpoints.'
      );
    }
    const [restEndpointObj, request] = createEndpointObject(cleanedUpState);
    if (isPageCreate) {
      createEndpoint(restEndpointObj, request, resetPageState);
      return;
    }
    if (!oldState || !oldState.current) {
      return;
    }
    editEndpoint(oldState.current, restEndpointObj, request, resetPageState);
  };

  return (
    <div className={styles.rest_create_layout}>
      <div className={styles.rest_heading_layout}>
        <BreadCrumb breadCrumbs={crumbs} />
        <h3 className={styles.rest_create_header}>
          {isPageCreate ? 'Create' : 'Edit'} Endpoint
        </h3>
      </div>
      <div className={styles.rest_form_layout}>
        <div className={styles.name_input_layout}>
          <Input
            label="Name"
            value={inputState.name}
            onChangeText={updateEndpointName}
            placeholder="Name"
          />
          <Input
            label="Description"
            value={inputState.comment}
            onChangeText={updateEndpointComment}
            placeholder="Description"
            type="textarea"
          />
        </div>
        <div className={styles.location_input_margin}>
          <Input
            label="Location"
            value={inputState.url}
            onChangeText={updateEndpointURL}
            placeholder="Location"
            note={locationNote}
          >
            <URLPreview urlInput={inputState.url} />
          </Input>
        </div>
        <MethodsInput
          currentState={inputState.methods}
          updateState={updateEndpointMethods}
        />
        <RequestViewer
          request={inputState.request}
          isEditable={!isPageCreate}
          onChangeQueryText={isPageCreate ? undefined : updateEndpointRequest}
        />
      </div>
      <div className={styles.rest_action_btns}>
        <Button color="white" onClick={resetPageState}>
          Cancel
        </Button>
        <Button
          color="yellow"
          onClick={onClickCreate}
          className={styles.create_btn_styles}
        >
          {isPageCreate ? 'Create' : 'Edit'} Endpoint
        </Button>
      </div>
    </div>
  );
};

const mapDispatchToProps = (dispatch: Dispatch) => ({
  routeToPage: (link: string) => dispatch(_push(link)),
  createEndpoint: (
    restEndpoint: RestEndpointEntry,
    request: string,
    cb: () => void
  ) => dispatch(addRESTEndpoint(restEndpoint, request, cb)),
  editEndpoint: (
    oldEntry: RestEndpointEntry,
    newEntry: RestEndpointEntry,
    request: string,
    cb: () => void
  ) => dispatch(editRESTEndpoint(oldEntry, newEntry, request, cb)),
  showError: (title: string, message?: string) =>
    dispatch(showErrorNotification(title, message)),
});
const mapStateToProps = (state: ReduxState) => ({
  metadataObject: state.metadata.metadataObject,
});
const createEndpointConnector = connect(mapStateToProps, mapDispatchToProps);
type InjectedProps = ConnectedProps<typeof createEndpointConnector>;

const ConnectedComponent = createEndpointConnector(CreateEndpoint);

export default ConnectedComponent;
