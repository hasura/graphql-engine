import Endpoints, { globalCookiePolicy } from '../../Endpoints';
import { UPDATE_DATA_HEADERS } from '../Services/Data/DataActions';
import { saveAdminSecretState } from '../AppState';
import { ADMIN_SECRET_HEADER_KEY, CLI_CONSOLE_MODE } from '../../constants';
import requestAction from '../../utils/requestAction';
import { Dispatch } from '../../types';
import globals from '../../Globals';

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
  const url = Endpoints.getSchema;
  const requestOptions: RequestInit = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: {
      [ADMIN_SECRET_HEADER_KEY]: adminSecret,
      'content-type': 'application/json',
    },
    body: JSON.stringify({
      type: 'select',
      args: {
        table: {
          name: 'hdb_table',
          schema: 'hdb_catalog',
        },
        columns: ['table_schema'],
        where: { table_schema: 'public' },
        limit: 1,
      },
    }),
  };

  dispatch(requestAction(url, requestOptions)).then(
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
