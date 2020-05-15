import fetch from 'isomorphic-fetch';
import { push } from 'react-router-redux';
import globals from 'Globals';

import {
  LOAD_REQUEST,
  DONE_REQUEST,
  FAILED_REQUEST,
  ERROR_REQUEST,
} from '../components/App/Actions';
import { globalCookiePolicy } from '../Endpoints';
import { parseResponseWithBigInt } from '../components/Main/utils';

const requestAction = (
  url,
  options,
  SUCCESS,
  ERROR,
  includeCredentials = true
) => {
  if (!options.credentials && includeCredentials) {
    options.credentials = globalCookiePolicy;
  }

  return dispatch => {
    return new Promise((resolve, reject) => {
      dispatch({ type: LOAD_REQUEST });
      fetch(url, options).then(
        response => {
          if (response.ok) {
            return response.text().then(res => {
              const results = parseResponseWithBigInt(res);
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
                if (window.location.pathname !== globals.urlPrefix + '/login') {
                  dispatch(push(globals.urlPrefix + '/login'));
                }
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
          console.error('Request error: ', error);
          dispatch({ type: FAILED_REQUEST });
          if (ERROR) {
            dispatch({
              type: ERROR,
              message: error.message,
              data: error.message,
            });
          }
          reject(error);
        }
      );
    });
  };
};

export default requestAction;
