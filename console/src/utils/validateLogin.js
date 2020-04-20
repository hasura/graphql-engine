import { clearAdminSecretState } from '../components/AppState';
import globals from '../Globals';

import { hyrateAdminSecret } from '../components/Login/Actions';

import { getAdminSecret } from '../components/Services/ApiExplorer/ApiRequest/utils';

const validateLogin = ({ dispatch }) => {
  return (nextState, replaceState, cb) => {
    // care about admin secret only if it is set
    if (globals.isAdminSecretSet) {
      const adminSecret = getAdminSecret();
      dispatch(hyrateAdminSecret(adminSecret, cb));
    } else {
      clearAdminSecretState();

      cb();
    }
  };
};

export default validateLogin;
