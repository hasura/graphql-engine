import Endpoints, { globalCookiePolicy } from '../../Endpoints';
import globals from '../../Globals';
import { updateDataHeaders } from '../Services/Data/DataActions';
import { saveAdminSecretState, loadAdminSecretState } from '../AppState';
import {
  getGraphiQLHeadersFromLocalStorage,
  setGraphiQLHeadersInLocalStorage,
} from '../Services/ApiExplorer/ApiRequest/utils';
import { setHeadersBulk } from '../Services/ApiExplorer/Actions';

const adminSecretKeyString = `x-hasura-${globals.adminSecretLabel}`;

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
      [adminSecretKeyString]: adminSecret,
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
          if (shouldPersist) {
            saveAdminSecretState(adminSecret);
          }
          dispatch(
            updateDataHeaders({
              'content-type': 'application/json',
              [adminSecretKeyString]: adminSecret,
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

export const handleHeaderInit = () => (dispatch, getState) => {
  const getDefaultGraphiqlHeaders = () => {
    return [
      {
        key: 'content-type',
        value: 'application/json',
        isActive: true,
        isNewHeader: false,
        isDisabled: false,
      },
    ];
  };

  const graphiqlHeaders =
    getGraphiQLHeadersFromLocalStorage() || getDefaultGraphiqlHeaders();

  let adminSecret = null;
  if (globals.consoleMode === 'server' && globals.isAdminSecretSet) {
    const adminSecretFromLS = loadAdminSecretState();
    const adminSecretInRedux = getState().tables.dataHeaders[
      adminSecretKeyString
    ];

    adminSecret = adminSecretFromLS || adminSecretInRedux;
  } else {
    adminSecret = globals.adminSecret;
  }

  if (adminSecret) {
    graphiqlHeaders.push({
      key: adminSecretKeyString,
      value: adminSecret,
      isActive: true,
      isNewHeader: false,
      isDisabled: true,
    });
  }

  // add an empty placeholder header
  graphiqlHeaders.push({
    key: '',
    value: '',
    isActive: true,
    isNewHeader: true,
    isDisabled: false,
  });

  // persist headers to local storage
  setGraphiQLHeadersInLocalStorage(graphiqlHeaders);

  // set headers in redux
  dispatch(setHeadersBulk(graphiqlHeaders));
};
