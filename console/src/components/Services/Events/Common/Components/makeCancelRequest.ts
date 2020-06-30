import { push } from 'react-router-redux';

import globals from '../../../../../Globals';
import { Thunk } from '../../../../../types';
import dataHeaders from '../../../../../components/Services/Data/Common/Headers';
import { globalCookiePolicy } from '../../../../../Endpoints';
import {
  showSuccessNotification,
  showErrorNotification,
} from '../../../Common/Notification';

const makeCancelRequest = (
  url: string,
  options: RequestInit,
  successText?: string,
  errorText?: string,
  successCallback?: () => void
): Thunk<Promise<any>> => {
  return (dispatch: any, getState: any) => {
    const requestOptions = { ...options };
    requestOptions.credentials = globalCookiePolicy;
    requestOptions.headers = {
      ...(options.headers || {}),
      ...dataHeaders(getState),
    };

    return new Promise((resolve, reject) => {
      fetch(url, requestOptions).then(
        response => {
          if (response.ok) {
            return response.json().then(results => {
              if (successText) {
                dispatch(showSuccessNotification(successText, results.message));
              }

              if (successCallback) {
                successCallback();
              }
              resolve(results);
            });
          }
          if (response.status >= 400 && response.status <= 500) {
            return response.json().then(errorMsg => {
              const msg = errorMsg;
              if (errorText) {
                dispatch(
                  showErrorNotification(errorText, 'Request was unsuccessful')
                );
              }
              if (msg.code && msg.code === 'access-denied') {
                if (window.location.pathname !== `${globals.urlPrefix}/login`) {
                  dispatch(push(`${globals.urlPrefix}/login`));
                }
              }
              reject(msg);
            });
          }
        },
        error => {
          console.error('Request error: ', error);
          if (errorText) {
            dispatch(showErrorNotification(errorText, error.message));
          }
          reject(error);
        }
      );
    });
  };
};

export default makeCancelRequest;
