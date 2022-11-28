import endpoints from '../../../../Endpoints';
import requestAction from '../../../../utils/requestAction';
import globals from '../../../../Globals';
import { loadPATState } from '../../../AppState';

export const getConfig = () => (dispatch, getState) => {
  const { dataHeaders } = getState().tables;
  const personalAccessToken = loadPATState();
  let updatedDataHeaders = {
    // 'content-type': 'application/json',
    ...dataHeaders,
  };
  if (personalAccessToken) {
    updatedDataHeaders = {
      // 'content-type': 'application/json',
      ...dataHeaders,
      [globals.patLabel]: `pat ${personalAccessToken}`,
    };
  }

  const options = {
    method: 'GET',
    headers: {
      ...updatedDataHeaders,
    },
  };

  return dispatch(requestAction(`${endpoints.serverConfig}`, options));
};
