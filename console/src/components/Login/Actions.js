import Endpoints, { globalCookiePolicy } from '../../Endpoints';
import push from 'react-router-redux';
import { UPDATE_DATA_HEADERS } from '../Services/Data/DataActions';
import { saveAdminSecretState } from '../AppState';
import { ADMIN_SECRET_HEADER_KEY } from '../../constants';
import requestAction from '../../utils/requestAction';
import globals from '../../Globals';

export const hyrateAdminSecret = (adminSecret, callback) => dispatch => {
  // set admin secret in globals
  globals.adminSecret = adminSecret;

  // set data headers in redux
  dispatch({
    type: UPDATE_DATA_HEADERS,
    data: {
      'content-type': 'application/json',
      [ADMIN_SECRET_HEADER_KEY]: adminSecret,
    },
  });
  if (callback) {
    callback();
  }
};

export const verifyLogin = ({
  adminSecret,
  shouldPersist,
  successCallback,
  errorCallback,
  dispatch,
}) => {
  const url = Endpoints.getSchema;
  const requestOptions = {
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
      if (successCallback) {
        successCallback();
      }
      if (adminSecret && shouldPersist) {
        saveAdminSecretState(adminSecret);
      }
      dispatch(hyrateAdminSecret(adminSecret, () => dispatch(push('/'))));
    },
    error => {
      errorCallback(error);
    }
  );
};
