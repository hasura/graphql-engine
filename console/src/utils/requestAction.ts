import { push } from 'react-router-redux';
import globals from '../Globals';
import { Thunk } from '../types';
import dataHeaders from '../components/Services/Data/Common/Headers';

import {
  LOAD_REQUEST,
  DONE_REQUEST,
  FAILED_REQUEST,
  ERROR_REQUEST,
  CONNECTION_FAILED,
} from '../components/App/Actions';
import { globalCookiePolicy } from '../Endpoints';

const requestAction = <T = any>(
  url: string,
  options: RequestInit = {},
  SUCCESS?: string,
  ERROR?: string,
  includeCredentials = true,
  includeAdminHeaders = false
): Thunk<Promise<T>> => {
  return (dispatch: any, getState: any) => {
    const requestOptions = { ...options };

    if (!options.credentials && includeCredentials) {
      requestOptions.credentials = globalCookiePolicy;
    }

    if (includeAdminHeaders) {
      requestOptions.headers = {
        ...(options.headers || {}),
        ...dataHeaders(getState),
      };
    }
    return new Promise((resolve, reject) => {
      dispatch({ type: LOAD_REQUEST });
      fetch(url, requestOptions).then(
        response => {
          const contentType = response.headers.get('Content-Type');
          const isResponseJson = `${contentType}`.includes('application/json');
          if (response.ok) {
            if (!isResponseJson) {
              return response.text().then(responseBody => {
                if (SUCCESS) {
                  dispatch({ type: SUCCESS, data: responseBody });
                }
                dispatch({ type: DONE_REQUEST });
                // TODO see how to improve here and remove the any
                resolve(responseBody as any);
              });
            }

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
            if (!isResponseJson) {
              return response.text().then(errorMessage => {
                if (ERROR) {
                  dispatch({ type: ERROR, data: errorMessage });
                } else {
                  dispatch({
                    type: ERROR_REQUEST,
                    data: errorMessage,
                    url,
                    params: options.body,
                    statusCode: response.status,
                  });
                }
                reject(errorMessage);
              });
            }
            return response.json().then(errorMsg => {
              const msg = errorMsg;
              if (ERROR) {
                dispatch({ type: ERROR, data: msg });
              } else {
                dispatch({
                  type: ERROR_REQUEST,
                  data: msg,
                  url,
                  params: options.body,
                  statusCode: response.status,
                });
              }
              if (msg.code && msg.code === 'access-denied') {
                if (window.location.pathname !== `${globals.urlPrefix}/login`) {
                  dispatch(push(`${globals.urlPrefix}/login`));
                }
              }
              reject(msg);
            });
          }
          return response.text().then(errorMsg => {
            dispatch({ type: FAILED_REQUEST });
            if (ERROR) {
              dispatch({ type: ERROR, response, data: errorMsg });
            }
            reject();
          });
        },
        error => {
          console.error('Request error: ', error);
          dispatch({ type: CONNECTION_FAILED });
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
