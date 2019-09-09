import Endpoints, { globalCookiePolicy } from '../../Endpoints';
import { updateDataHeaders } from '../Services/Data/DataActions';
import { saveAdminSecretState } from '../AppState';
import { ADMIN_SECRET_HEADER_KEY } from '../../constants';
import { SET_ADMIN_SECRET } from '../Main/Actions';

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
  fetch(url, requestOptions)
    .then(response => {
      if (response.status === 200) {
        if (adminSecret) {
          // set admin secret to local storage
          if (shouldPersist) {
            saveAdminSecretState(adminSecret);
          }

          // set admin secret in redux
          dispatch({
            type: SET_ADMIN_SECRET,
            data: adminSecret,
          });

          // set data headers in redux
          dispatch(
            updateDataHeaders({
              'content-type': 'application/json',
              [ADMIN_SECRET_HEADER_KEY]: adminSecret,
            })
          );
        }
        if (successCallback) {
          successCallback();
        }
      } else {
        errorCallback(true);
      }
    })
    .catch(error => {
      console.error(error);
      errorCallback(error);
    });
};
