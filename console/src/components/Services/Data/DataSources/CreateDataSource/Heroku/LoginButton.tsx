import * as React from 'react';
import { HerokuSession } from './types';
import { Dispatch } from '../../../../../../types';
import { setHerokuSession as updateHerokuSession } from '../../../../../Main/Actions';
import Globals from '../../../../../../Globals';
import { showErrorNotification } from '../../../../Common/Notification';
import {
  exchangeHerokuCode,
  generateRandomString,
  getPersistedHerokuCallbackSearch,
  clearPersistedHerokuCallbackSearch,
} from './utils';

const HEROKU_OAUTH_CLIENT_ID = Globals.herokuOAuthClientId;

export const openPopup = (
  setCode: (code: string | null) => void,
  setLoading: (loading: boolean) => void,
  setError: (message: string | null) => void
) => () => {
  if (!HEROKU_OAUTH_CLIENT_ID) {
    setError(
      'Could not login with Heroku. Heroku OAuth Client ID not configured.'
    );
    return;
  }

  // generat a random anti-forgery token
  const requestState = generateRandomString();

  let received = false;

  // update heroku code value after fetching the callback search value from local storage
  const updateToken = (token: string) => {
    received = true;
    const searchParams = new URLSearchParams(token);
    const responseState = searchParams.get('state');
    if (responseState !== requestState) {
      setLoading(false);
      return;
    }
    setCode(searchParams.get('code'));
    // clear heroku callback search value from local storage after integration
    clearPersistedHerokuCallbackSearch();
    setLoading(false);
  };

  // open popup
  setLoading(true);
  setError(null);
  const popup = window.open(
    `https://id.heroku.com/oauth/authorize?client_id=${HEROKU_OAUTH_CLIENT_ID}&response_type=code&scope=global&state=${requestState}`,
    'heroku-auth',
    'menubar=no,toolbar=no,location=no,width=800,height=600'
  );

  if (popup) {
    const intervalId = setInterval(() => {
      if (popup.closed) {
        setLoading(false);
        setTimeout(() => {
          // fetch heroku callback search value from local storage
          const token = getPersistedHerokuCallbackSearch();
          if (token) {
            updateToken(token);
          }
          if (!received) {
            setError('Unexpected error. Please try again.');
          }
        }, 500);
        clearInterval(intervalId);
      }
    }, 500);
  }
};

type Props = {
  dispatch: Dispatch;
  callback?: VoidFunction;
};

// A button button that performs login and makes a callback with session
const LoginButton: React.FC<Props> = ({ children, dispatch, callback }) => {
  // state for oauth authorization code
  const [code, setCode] = React.useState<string | null>(null);

  // loading state for heroku auth
  const [authenticating, setAuthenticating] = React.useState(false);

  // loading state for token exchange
  const [exchanging, setExchanging] = React.useState(false);

  // error state
  const [error, setError] = React.useState<string | null>(null);

  // initialise mutation to exchange auth_code for access token
  React.useEffect(() => {
    if (code) {
      setExchanging(true);
      exchangeHerokuCode(code)
        .then((session: HerokuSession) => {
          dispatch(updateHerokuSession(session));
          if (callback) {
            callback();
          }
        })
        .catch(e => {
          setError(
            `Error logging in with Heroku: ${e.message || 'unexpected'}`
          );
        })
        .finally(() => {
          setExchanging(false);
        });
    }
  }, [code]);

  // If an error occurs, toast it
  React.useEffect(() => {
    if (error) {
      dispatch(showErrorNotification('Error logging in with Heroku', error));
    }
  }, [error]);

  const loading = authenticating || exchanging;

  return (
    <div
      role="button"
      onClick={() => {
        if (loading) return;
        openPopup(setCode, setAuthenticating, setError)();
      }}
      style={{ width: 'auto' }}
    >
      {children}
    </div>
  );
};

export default LoginButton;
