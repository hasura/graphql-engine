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
import Endpoints, { globalCookiePolicy } from '../Endpoints';
import { processResponseDetails } from '../components/Services/ApiExplorer/Actions';

// if URL is graphql, and body is a query and not a mutation
const checkCachable = (url: string, body: BodyInit | null | undefined) => {
  if (url === Endpoints.graphQLUrl) {
    if (body && typeof body === 'string') {
      // get query and operation name from body init
      const bodyObj = JSON.parse(body);
      if (bodyObj.operationName && bodyObj.query) {
        const queryRegex = new RegExp(
          `query\\s+${bodyObj.operationName}\\s*`,
          'i'
        );
        const isQuery = queryRegex.test(bodyObj.query as string);
        return isQuery;
      }
    }
  }
  return false;
};

const getCacheRequestWarning = (
  warningHeader: string | null
): string | null => {
  if (!warningHeader) {
    return null;
  }
  return [
    'cache-store-size-limit-exceeded',
    'cache-store-capacity-exceeded',
    'cache-store-error',
  ]?.some(warning => warningHeader?.includes(warning))
    ? warningHeader
    : null;
};
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
        async response => {
          const endTime = new Date().getTime();
          const responseSize = (await response.clone().text()).length;
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
                const responseTimeMs = endTime - startTime;
                const isResponseCached = response.headers.has('Cache-Control');
                const cacheWarning = getCacheRequestWarning(
                  response.headers.get('Warning')
                );
                const isRequestCachable = checkCachable(
                  url,
                  requestOptions.body
                );
                dispatch(
                  processResponseDetails(
                    responseTimeMs,
                    responseSize,
                    isResponseCached,
                    requestTrackingId,
                    cacheWarning,
                    isRequestCachable
                  )
                );
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
