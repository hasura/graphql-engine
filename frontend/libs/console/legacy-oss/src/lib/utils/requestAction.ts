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
import { processResponseDetails } from '../components/Services/ApiExplorer/Actions';
import { maskPostgresError } from '../components/Services/Data/DataSources/SampleDatabase/service';

const requestAction = <T = any>(
  url: string,
  options: RequestInit = {},
  SUCCESS?: string,
  ERROR?: string,
  includeCredentials = true,
  includeAdminHeaders = false,
  requestTrackingId?: string
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
      const startTime = new Date().getTime();
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

              if (requestTrackingId) {
                const endTime = new Date().getTime();
                const responseTimeMs = endTime - startTime;
                const isResponseCached = response.headers.has('Cache-Control');
                const responseSize = JSON.stringify(results).length * 2;
                dispatch(
                  processResponseDetails(
                    responseTimeMs,
                    responseSize,
                    isResponseCached,
                    requestTrackingId
                  )
                );
              }

              // if GraphQL error, mask it if it's a postgres trial read-only DB error
              if (results?.errors) {
                const maskedErrorMsg = maskPostgresError(results, getState);
                if (maskedErrorMsg) {
                  const graphqlError: any = {
                    errors: [{ message: maskedErrorMsg }],
                  };
                  resolve(graphqlError);
                }
              }

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
              // if it's a postgres trial read-only DB error, mask it with a friendly error
              const maskedErrorMsg = maskPostgresError(errorMsg, getState);
              if (maskedErrorMsg) {
                reject(maskedErrorMsg);
              }

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
