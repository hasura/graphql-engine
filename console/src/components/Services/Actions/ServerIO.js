import Endpoints, { globalCookiePolicy } from '../../../Endpoints';
import requestAction from '../../../utils/requestAction';
import dataHeaders from '../Data/Common/Headers';
import {
  LOADING_ACTIONS,
  LOADING_ACTIONS_SUCCESS,
  LOADING_ACTIONS_FAILURE,
} from './reducer';
import { filterInconsistentMetadataObjects } from '../Settings/utils';
import { makeMigrationCall } from '../Data/DataActions';
import {
  generateSetCustomTypesQuery,
  generateCreateActionQuery,
  generateDropActionQuery,
} from '../../Common/utils/v1QueryUtils';
import {
  reformCustomTypes,
  generateActionDefinition,
  getStateValidationError,
} from './Common/utils';
import { showErrorNotification } from '../Common/Notification';

export const fetchActions = () => {
  return (dispatch, getState) => {
    const url = Endpoints.getSchema;
    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify({
        type: 'select',
        args: {
          table: {
            name: 'actions',
            schema: 'hdb_catalog',
          },
          columns: ['*'],
          order_by: [{ column: 'name', type: 'asc', nulls: 'last' }],
        },
      }),
    };
    dispatch({ type: LOADING_ACTIONS });
    return dispatch(requestAction(url, options)).then(
      data => {
        let consistentActions = data;
        const { inconsistentObjects } = getState().metadata;

        if (inconsistentObjects.length > 0) {
          consistentActions = filterInconsistentMetadataObjects(
            data,
            inconsistentObjects,
            'actions'
          );
        }

        dispatch({ type: LOADING_ACTIONS_SUCCESS, data: consistentActions });

        return Promise.resolve();
      },
      error => {
        console.error('Failed to load actions' + JSON.stringify(error));
        dispatch({ type: LOADING_ACTIONS_FAILURE, error });
        return Promise.reject();
      }
    );
  };
};

export const createAction = () => (dispatch, getState) => {
  const { add: state } = getState().actions;
  const { types: existingTypes } = getState().types;

  // TODO generate down migration for custom types

  const validationError = getStateValidationError(state);
  if (validationError) {
    return dispatch(showErrorNotification(validationError));
  }

  const customFieldsQueryUp = generateSetCustomTypesQuery(
    reformCustomTypes(state.types)
  );

  const customFieldsQueryDown = generateSetCustomTypesQuery(existingTypes);

  const actionQueryUp = generateCreateActionQuery(
    state.name,
    generateActionDefinition(state)
  );

  const actionQueryDown = generateDropActionQuery(state.name);

  const upQueries = [customFieldsQueryUp, actionQueryUp];
  const downQueries = [customFieldsQueryDown, actionQueryDown];

  const migrationName = `create_action_${state.name}`;
  const requestMsg = 'Creating action...';
  const successMsg = 'Created action successfully';
  const errorMsg = 'Creating action failed';
  const customOnSuccess = () => {};
  const customOnError = () => {};

  makeMigrationCall(
    dispatch,
    getState,
    upQueries,
    downQueries,
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg
  );
};
