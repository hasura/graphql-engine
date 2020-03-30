import { push } from 'react-router-redux';
import globals from '../Globals';

import {
  LOAD_REQUEST,
  DONE_REQUEST,
  FAILED_REQUEST,
  ERROR_REQUEST,
} from '../components/App/Actions';

const requestAction = (
  url: string,
  options: RequestInit,
  SUCCESS?: string,
  ERROR?: string,
  includeCredentials=true
) => {
  if (!options.credentials && includeCredentials) {
    options.credentials = 'omit';
  }

  return (dispatch: any) => {
    return new Promise((resolve, reject) => {
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
            return response.json().then(errorMsg => {
              let msg = errorMsg;
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
