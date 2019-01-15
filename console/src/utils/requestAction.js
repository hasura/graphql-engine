import fetch from 'isomorphic-fetch';
import { push } from 'react-router-redux';
import globals from 'Globals';
import { UPDATE_DATA_HEADERS } from 'components/Services/Data/DataActions';

import {
  LOAD_REQUEST,
  DONE_REQUEST,
  FAILED_REQUEST,
  ERROR_REQUEST,
  CONNECTION_FAILED,
} from 'components/App/Actions';

import { LOGIN_IN_PROGRESS, LOGIN_ERROR } from 'components/Main/Actions';

const requestAction = (
  url,
  options,
  SUCCESS,
  ERROR,
  includeCredentials = true
) => {
  if (!options.credentials && includeCredentials) {
    options.credentials = 'omit';
  }

  return dispatch => {
    const p1 = new Promise((resolve, reject) => {
      dispatch({ type: LOAD_REQUEST });
      fetch(url, options).then(
        response => {
          if (response.ok) {
            return response.json().then(results => {
              if (SUCCESS) {
                dispatch({ type: SUCCESS, data: results });
              }
              dispatch({ type: DONE_REQUEST });
              resolve(results);
            });
          }
          dispatch({ type: FAILED_REQUEST });
          if (response.status >= 400 && response.status <= 500) {
            return response.text().then(errorMsg => {
              let msg = errorMsg;
              try {
                msg = JSON.parse(errorMsg);
              } catch (e) {
                msg = errorMsg;
              }
              if (ERROR) {
                dispatch({ type: ERROR, data: msg });
              } else {
                dispatch({
                  type: ERROR_REQUEST,
                  data: msg,
                  url: url,
                  params: options.body,
                  statusCode: response.status,
                });
              }
              if (msg.code && msg.code === 'access-denied') {
                dispatch({
                  type: UPDATE_DATA_HEADERS,
                  data: {
                    'Content-Type': 'application/json',
                    'X-Hasura-Access-Key': globals.accessKey,
                  },
                });
                dispatch({ type: LOGIN_IN_PROGRESS, data: false });
                dispatch({ type: LOGIN_ERROR, data: false });
                dispatch(push(globals.urlPrefix + '/login'));
              }
              reject(msg);
            });
          }
          response.text().then(errorMsg => {
            dispatch({ type: FAILED_REQUEST });
            if (ERROR) {
              dispatch({ type: ERROR, response, data: errorMsg });
            }
            reject();
          });
        },
        error => {
          console.error(error);
          dispatch({ type: FAILED_REQUEST });
          dispatch({ type: CONNECTION_FAILED });
          if (ERROR) {
            dispatch({
              type: ERROR,
              code: 'server-connection-failed',
              message: error.message,
              data: error.message,
            });
          }
          reject(error);
        }
      );
    });
    return p1;
  };
};

export default requestAction;
