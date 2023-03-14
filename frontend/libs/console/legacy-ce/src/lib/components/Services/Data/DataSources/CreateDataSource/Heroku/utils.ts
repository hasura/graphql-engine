/* eslint-disable no-loop-func */

import React from 'react';
import {
  HerokuSession,
  HerokuApp,
  HerokuError,
  HerokuConfigVars,
  ProgressState,
  StateDetails,
} from './types';
import { Dispatch } from '../../../../../../types';
import { showErrorNotification } from '../../../../Common/Notification';
import Endpoints from '../../../../../../Endpoints';
import Globals from '../../../../../../Globals';
import {
  getLSItem,
  LS_KEYS,
  removeLSItem,
  setLSItem,
} from '../../../../../../utils/localStorage';

export {
  getEnvVars,
  updateEnvVars,
  getAvailableEnvVar,
  setDBURLInEnvVars,
  verifyProjectHealthAndConnectDataSource,
} from '../utils';

export const getHerokuHeaders = (session: HerokuSession) => ({
  authorization: `${session.token_type} ${session.access_token}`,
  'content-type': 'application/json',
  accept: 'application/vnd.heroku+json; version=3',
});

const createHerokuApp = (session: HerokuSession) => {
  return fetch('https://api.heroku.com/apps', {
    method: 'POST',
    headers: getHerokuHeaders(session),
    body: '{}',
  })
    .then(httpResp => {
      if (httpResp.status < 300) {
        return httpResp.json().then((response: HerokuApp) => {
          return response;
        });
      }
      return httpResp
        .json()
        .then((errorBody: HerokuError) => {
          throw new Error(`Error from Heroku: ${errorBody.message || ''}`);
        })
        .catch(e => {
          throw e;
        });
    })
    .catch(e => {
      throw e;
    });
};

const installPostgresOnApp = (herokuApp: HerokuApp, session: HerokuSession) => {
  return fetch(`https://api.heroku.com/apps/${herokuApp.name}/addons`, {
    method: 'POST',
    headers: getHerokuHeaders(session),
    body: JSON.stringify({
      plan: 'heroku-postgresql:hobby-dev',
    }),
  }).then(httpResp => {
    if (httpResp.status >= 300) {
      return httpResp.json().then((errorBody: HerokuError) => {
        throw new Error(`Error from Heroku: ${errorBody.message || ''}`);
      });
    }
    return Promise.resolve();
  });
};

export const getAppConfigVars = (
  herokuApp: HerokuApp,
  session: HerokuSession
) => {
  return fetch(`https://api.heroku.com/apps/${herokuApp.name}/config-vars`, {
    method: 'GET',
    headers: getHerokuHeaders(session),
  }).then(httpResp => {
    if (httpResp.status < 300) {
      return httpResp.json().then((responseBody: HerokuConfigVars) => {
        return responseBody;
      });
    }
    if (httpResp.status === 401) {
      throw new Error(`Unauthorised. Please login with Heroku again`);
    }
    return httpResp.json().then((errorBody: HerokuError) => {
      throw new Error(`Error from Heroku: ${errorBody.message || ''}`);
    });
  });
};

const initialState: ProgressState = {
  'creating-app': {
    status: 'pending',
  },
  'installing-postgres': {
    status: 'pending',
  },
  'getting-config': {
    status: 'pending',
  },
};

export const useHerokuDBCreation = (
  session: HerokuSession,
  autoStart: boolean,
  dispatch: Dispatch
) => {
  const [state, setState] = React.useState(initialState);
  const [error, setError] = React.useState<string | null>(null);

  const start = (sess: HerokuSession) => {
    setState(initialState);
    setError(null);
    setState(s => ({ ...s, 'creating-app': { status: 'in-progress' } }));
    createHerokuApp(sess)
      .then(herokuApp => {
        if (!herokuApp) {
          return;
        }
        setState(s => ({
          ...s,
          'creating-app': {
            status: 'success',
            details: herokuApp,
          },
          'installing-postgres': {
            status: 'in-progress',
          },
        }));
        installPostgresOnApp(herokuApp, sess).then(() => {
          setState(s => ({
            ...s,
            'installing-postgres': {
              status: 'success',
              details: undefined,
            },
            'getting-config': {
              status: 'in-progress',
            },
          }));
          getAppConfigVars(herokuApp, sess).then(configVars => {
            if (!configVars) {
              return;
            }
            setState(s => ({
              ...s,
              'getting-config': {
                status: 'success',
                details: configVars,
              },
            }));
          });
        });
      })
      .catch(e => {
        setError(e.message);
      });
  };

  React.useEffect(() => {
    if (error) {
      const [title, message] = error.split(': ');
      dispatch(showErrorNotification(title, message));
      const errorStatus: StateDetails<HerokuError> = {
        status: 'failed',
        details: {
          message: error,
        },
      };
      if (state['creating-app'].status === 'in-progress') {
        setState(s => ({ ...s, 'creating-app': errorStatus }));
      }
      if (state['installing-postgres'].status === 'in-progress') {
        setState(s => ({ ...s, 'installing-postgres': errorStatus }));
      }
      if (state['getting-config'].status === 'in-progress') {
        setState(s => ({ ...s, 'getting-config': errorStatus }));
      }
    }
  }, [error]);

  React.useEffect(() => {
    if (autoStart) {
      start(session);
    }
  }, [autoStart]);

  return {
    state,
    start,
    error,
    inProgress:
      state['creating-app'].status === 'in-progress' ||
      state['installing-postgres'].status === 'in-progress' ||
      state['getting-config'].status === 'in-progress',
  };
};

export const exchangeHerokuCode = (code: string) => {
  const query = `
    mutation exchangeHerokuToken($code: String!) {
      herokuTokenExchange(payload: { type: code, value: $code }) {
        access_token
        expires_in
        token_type
        refresh_token
      }
    }
  `;
  const variables = {
    code,
  };
  return fetch(Endpoints.luxDataGraphql, {
    method: 'POST',
    headers: {
      'content-type': 'application/json',
    },
    credentials: 'include',
    body: JSON.stringify({
      query,
      variables,
    }),
  })
    .then(r => {
      if (r.status >= 300) {
        throw new Error('Invalid login. Please try again.');
      }
      return r.json().then(response => {
        if (response.errors) {
          throw new Error(response.errors[0]?.message || 'Unexpected');
        }
        return response.data.herokuTokenExchange as HerokuSession;
      });
    })
    .catch(e => {
      throw e;
    });
};

export const clearPersistedHerokuCallbackSearch = () => {
  removeLSItem(LS_KEYS.herokuCallbackSearch);
};

export const persistHerokuCallbackSearch = (value: string) => {
  setLSItem(LS_KEYS.herokuCallbackSearch, value);
};

export const getPersistedHerokuCallbackSearch = () => {
  return getLSItem(LS_KEYS.herokuCallbackSearch);
};

export const startHerokuDBURLSync = (
  envVar: string,
  appName: string,
  appID: string
) => {
  const projectID = Globals.hasuraCloudProjectId;
  const query = `
    mutation startDBURLSync (
      $appName: String!
      $appID:String!
      $projectID: uuid!
      $env: String!
    ) {
      herokuRegisterWebhookVar(
        varName: $env,
        appID: $appID,
        projectID: $projectID,
        appName: $appName
      ) {
        status
      }
    }

  `;
  const variables = {
    appName,
    appID,
    projectID,
    env: envVar,
  };
  return fetch(Endpoints.luxDataGraphql, {
    method: 'POST',
    headers: {
      'content-type': 'application/json',
    },
    credentials: 'include',
    body: JSON.stringify({
      query,
      variables,
    }),
  })
    .then(r => {
      return r.json().then(({ data, errors }) => {
        if (errors) {
          throw new Error(errors[0]?.message || 'unexpected');
        }
        return data;
      });
    })
    .catch(e => {
      console.error('Failed to start database URL sync', e.message);
    });
};
