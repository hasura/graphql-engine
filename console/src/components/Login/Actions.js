import Endpoints, { globalCookiePolicy } from '../../Endpoints';
import { push } from 'react-router-redux';
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
  console.log(adminSecret);
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
        successCallback();
        if (shouldPersist) {
          saveAdminSecretState(adminSecret);
        }
        dispatch(
          updateDataHeaders({
            'content-type': 'application/json',
            [adminSecretKeyString]: adminSecret,
          })
        );
        dispatch(push(globals.urlPrefix));
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
  // get headers from local storage and parse them, set default headers if they are not parseable
  const graphiqlHeadersFromLocalStorage = getGraphiQLHeadersFromLocalStorage();
  const defaultGraphiqlHeaders = [
    {
      key: 'content-type',
      value: 'application/json',
      isActive: true,
      isNewHeader: false,
      isDisabled: false,
    },
  ];
  let graphiqlHeaders;
  try {
    graphiqlHeaders = graphiqlHeadersFromLocalStorage
      ? JSON.parse(graphiqlHeadersFromLocalStorage)
      : defaultGraphiqlHeaders;
  } catch (e) {
    graphiqlHeaders = defaultGraphiqlHeaders;
  }

  // add an empty placeholder header if not present
  const lastHeader = graphiqlHeaders[graphiqlHeaders.length - 1];
  if (lastHeader.key) {
    graphiqlHeaders.push({
      key: '',
      value: '',
      isActive: true,
      isNewHeader: true,
      isDisabled: false,
    });
  }

  // persist these headers back to local storage
  setGraphiQLHeadersInLocalStorage(JSON.stringify(graphiqlHeaders));

  //append admin secret to headers
  if (globals.consoleMode === 'server' && globals.isAdminSecretSet) {
    const adminSecretFromLs = loadAdminSecretState();
    if (adminSecretFromLs) {
      graphiqlHeaders = [
        ...graphiqlHeaders.slice(0, graphiqlHeaders.length - 1),
        {
          key: adminSecretKeyString,
          value: adminSecretFromLs,
          isActive: true,
          isNewHeader: false,
          isDisabled: true,
        },
        graphiqlHeaders[graphiqlHeaders.length - 1],
      ];
    } else {
      const adminSecretInRedux = getState().tables.dataHeaders[
        adminSecretKeyString
      ];
      graphiqlHeaders = [
        ...graphiqlHeaders.slice(0, graphiqlHeaders.length - 1),
        {
          key: adminSecretKeyString,
          value: adminSecretInRedux,
          isActive: true,
          isNewHeader: false,
          isDisabled: true,
        },
        graphiqlHeaders[graphiqlHeaders.length - 1],
      ];
    }
  } else if (globals.adminSecret) {
    graphiqlHeaders = [
      ...graphiqlHeaders.slice(0, graphiqlHeaders.length - 1),
      {
        key: adminSecretKeyString,
        value: globals.adminSecret,
        isActive: true,
        isNewHeader: false,
        isDisabled: true,
      },
      graphiqlHeaders[graphiqlHeaders.length - 1],
    ];
  }

  // set headers in redux
  dispatch(setHeadersBulk(graphiqlHeaders));
};
