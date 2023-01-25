import Endpoints, { globalCookiePolicy } from '../../Endpoints';
import { UPDATE_DATA_HEADERS } from '../Services/Data/DataActions';
import { saveAdminSecretState } from '../AppState';
import { ADMIN_SECRET_HEADER_KEY, CLI_CONSOLE_MODE } from '../../constants';
import requestAction from '../../utils/requestAction';
import { Dispatch } from '../../types';
import globals from '../../Globals';
import { inconsistentObjectsQuery } from '../../metadata/queryUtils';

type VerifyLoginOptions = {
  adminSecret: string;
  shouldPersist: boolean;
  successCallback: () => void;
  errorCallback: (err: Error) => void;
  dispatch: Dispatch;
};

export const verifyLogin = ({
  adminSecret,
  shouldPersist,
  successCallback,
  errorCallback,
  dispatch,
}: VerifyLoginOptions) => {
  const options: RequestInit = {
    method: 'POST',
    credentials: globalCookiePolicy,
    headers: {
      [ADMIN_SECRET_HEADER_KEY]: adminSecret,
      'content-type': 'application/json',
    },
    body: JSON.stringify(inconsistentObjectsQuery),
  };
  return dispatch(requestAction(Endpoints.metadata, options)).then(
    () => {
      if (adminSecret) {
        if (globals.consoleMode !== CLI_CONSOLE_MODE) {
          // set admin secret to local storage
          if (shouldPersist) {
            saveAdminSecretState(adminSecret);
          }

          // set admin secret in globals
          globals.adminSecret = adminSecret;
        }

        // set data headers in redux
        dispatch({
          type: UPDATE_DATA_HEADERS,
          data: {
            'content-type': 'application/json',
            [ADMIN_SECRET_HEADER_KEY]: adminSecret,
          },
        });
      }
      if (successCallback) {
        successCallback();
      }
    },
    error => {
      errorCallback(error);
    }
  );
};
