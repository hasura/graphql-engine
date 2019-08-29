import { clearAdminSecretState } from '../components/AppState';
import globals from '../Globals';

import { verifyLogin } from '../components/Login/Actions';

import { getAdminSecret } from '../components/Services/ApiExplorer/ApiRequest/utils';

const validateLogin = ({ dispatch }) => {
  return (nextState, replaceState, cb) => {
    // care about admin secret only if it is set
    if (globals.isAdminSecretSet || globals.adminSecret) {
      const validationSuccessCallback = () => {
        if (nextState.location.pathname === '/login') {
          replaceState('/');
        }
        cb();
      };

      const validationFailureCallback = () => {
        clearAdminSecretState();
        if (nextState.location.pathname !== '/login') {
          replaceState('/login');
        }
        cb();
      };

      const adminSecret = getAdminSecret();

      verifyLogin({
        adminSecret,
        successCallback: validationSuccessCallback,
        errorCallback: validationFailureCallback,
        dispatch,
      });
    } else {
      cb();
    }
  };
};

export default validateLogin;
