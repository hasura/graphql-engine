import { push } from 'react-router-redux';
import globals from '../../Globals';
import defaultState from './State';
import requestAction from '../../utils/requestAction';
import requestActionPlain from '../../utils/requestActionPlain';
import Endpoints, { globalCookiePolicy } from '../../Endpoints';
import {
  saveAdminSecretState,
  savePATState,
  clearPATState,
  loadPATState,
  loadAdminSecretState,
} from '../AppState';
import {
  ADMIN_SECRET_ERROR,
  clearAdminSecretState,
  UPDATE_DATA_HEADERS,
} from '@hasura/console-legacy-ce';
import { getFeaturesCompatibility } from '../../helpers/versionUtils';
import {
  changeRequestHeader,
  removeRequestHeader,
  showErrorNotification,
  mainState,
} from '@hasura/console-legacy-ce';
import {
  CONSTANT_HEADERS,
  SERVER_CONSOLE_MODE,
  CLIENT_NAME_HEADER,
  CLIENT_NAME_HEADER_VALUE,
} from '../../constants';
import { mainReducer, isCloudConsole } from '@hasura/console-legacy-ce';
import { getKeyFromLS, initLS } from '../Login/localStorage';
import { parseQueryParams } from '../Login/utils';
import upsertToLS, { removeHeaderFromLS } from '../../utils/upsertToLS';
import { constructRedirectUrl } from '../../utils/utils';
import { retrieveByRefreshToken } from '../OAuthCallback/Actions';
import { decodeToken } from '../../utils/computeAccess';
import extendedGlobals from '../../Globals';

const UPDATE_HASURA_DOT_COM_ACCESS = 'Main/UPDATE_HASURA_DOT_COM_ACCESS';
const SET_MIGRATION_STATUS_SUCCESS = 'Main/SET_MIGRATION_STATUS_SUCCESS';
const SET_MIGRATION_STATUS_ERROR = 'Main/SET_MIGRATION_STATUS_ERROR';
const SET_SERVER_VERSION_SUCCESS = 'Main/SET_SERVER_VERSION_SUCCESS';
const SET_SERVER_VERSION_ERROR = 'Main/SET_SERVER_VERSION_ERROR';
const SET_LATEST_SERVER_VERSION_SUCCESS =
  'Main/SET_LATEST_SERVER_VERSION_SUCCESS';
const SET_LATEST_SERVER_VERSION_ERROR = 'Main/SET_LATEST_SERVER_VERSION_ERROR';
const UPDATE_MIGRATION_STATUS_SUCCESS = 'Main/UPDATE_MIGRATION_STATUS_SUCCESS';
const UPDATE_MIGRATION_STATUS_ERROR = 'Main/UPDATE_MIGRATION_STATUS_ERROR';
const HASURACTL_URL_ENV = 'Main/HASURACTL_URL_ENV';
const UPDATE_MIGRATION_MODE = 'Main/UPDATE_MIGRATION_MODE';
const UPDATE_MIGRATION_MODE_PROGRESS = 'Main/UPDATE_MIGRATION_MODE_PROGRESS';
const EXPORT_METADATA_SUCCESS = 'Main/EXPORT_METADATA_SUCCESS';
const EXPORT_METADATA_ERROR = 'Main/EXPORT_METADATA_ERROR';
const UPDATE_ADMIN_SECRET_INPUT = 'Main/UPDATE_ADMIN_SECRET_INPUT';
const LOGIN_IN_PROGRESS = 'Main/LOGIN_IN_PROGRESS';
const LOGIN_ERROR = 'Main/LOGIN_ERROR';
const UPDATE_PERSONAL_ACCESS_TOKEN = 'Main/UPDATE_PERSONAL_ACCESS_TOKEN';
const UPDATE_PROJECT_ID = 'Main/UPDATE_PROJECT_ID';
const UPDATE_PROJECT_NAME = 'Main/UPDATE_PROJECT_NAME';
const FETCHING_LUX_PROJECT_INFO = 'Main/FETCHING_LUX_PROJECT_INFO';
const FETCHED_LUX_PROJECT_INFO = 'Main/FETCHED_LUX_PROJECT_INFO';
const ERROR_FETCHING_LUX_PROJECT_INFO = 'Main/ERROR_FETCHING_LUX_PROJECT_INFO';
const FETCHING_LUX_PROJECT_ENTITLEMENTS =
  'Main/FETCHING_LUX_PROJECT_ENTITLEMENTS';
const FETCHED_LUX_PROJECT_ENTITLEMENTS =
  'Main/FETCHED_LUX_PROJECT_ENTITLEMENTS';
const ERROR_FETCHING_LUX_PROJECT_ENTITLEMENTS =
  'Main/ERROR_FETCHING_LUX_PROJECT_ENTITLEMENTS';

export const SET_METADATA = 'Main/SET_METADATA';
export const SET_METADATA_LOADING = 'Main/SET_METADATA_LOADING';

/* Server config constants*/
const FETCHING_SERVER_CONFIG = 'Main/FETCHING_SERVER_CONFIG';
const SERVER_CONFIG_FETCH_SUCCESS = 'Main/SERVER_CONFIG_FETCH_SUCCESS';
const SERVER_CONFIG_FETCH_FAIL = 'Main/SERVER_CONFIG_FETCH_FAIL';
/* End */
const SET_FEATURES_COMPATIBILITY = 'Main/SET_FEATURES_COMPATIBILITY';
const setFeaturesCompatibility = data => ({
  type: SET_FEATURES_COMPATIBILITY,
  data,
});

const featureCompatibilityInit = () => {
  return (dispatch, getState) => {
    const { serverVersion } = getState().main;

    if (!serverVersion) {
      return;
    }

    const featuresCompatibility = getFeaturesCompatibility(serverVersion);

    return dispatch(setFeaturesCompatibility(featuresCompatibility));
  };
};

const loadMigrationStatus = () => dispatch => {
  const url = Endpoints.hasuractlMigrateSettings;
  const options = {
    method: 'GET',
    credentials: globalCookiePolicy,
    headers: { 'content-type': 'application/json' },
  };
  return dispatch(
    requestAction(
      url,
      options,
      SET_MIGRATION_STATUS_SUCCESS,
      SET_MIGRATION_STATUS_ERROR
    )
  );
};

const refetchMetadata = () => (dispatch, getState) => {
  const url = Endpoints.metadata;
  const options = {
    method: 'POST',
    credentials: globalCookiePolicy,
    headers: getState().tables.dataHeaders,
    body: JSON.stringify({ type: 'export_metadata', args: {} }),
  };
  dispatch({
    type: SET_METADATA_LOADING,
    data: true,
  });
  return dispatch(requestAction(url, options)).then(
    data =>
      dispatch({
        type: SET_METADATA,
        data: { ...data, loading: false },
      }),
    console.error
  );
};

export const getMetricConfigPayload = ({
  analyze_query_variables,
  analyze_response_body,
}) => {
  const payload = {
    type: 'set_metrics_config',
    args: {
      analyze_query_variables,
      analyze_response_body,
    },
  };
  return payload;
};

export const setMetricConfigAction =
  (analyze_query_variables, analyze_response_body) => (dispatch, getState) => {
    const url = Endpoints.metadata;
    const options = {
      method: 'POST',
      credentials: globalCookiePolicy,
      headers: getState().tables.dataHeaders,
      body: JSON.stringify(
        getMetricConfigPayload({
          analyze_query_variables,
          analyze_response_body,
        })
      ),
    };
    return dispatch(requestAction(url, options));
  };

const setApiLimits = payload => (dispatch, getState) => {
  const url = Endpoints.metadata;
  const options = {
    method: 'POST',
    credentials: globalCookiePolicy,
    headers: getState().tables.dataHeaders,
    body: JSON.stringify(payload),
  };
  return dispatch(requestAction(url, options));
};

const loadServerVersion = () => dispatch => {
  const url = Endpoints.version;
  const options = {
    method: 'GET',
    credentials: globalCookiePolicy,
    headers: { 'content-type': 'application/json' },
  };
  return dispatch(requestActionPlain(url, options)).then(
    data => {
      let parsedVersion;
      try {
        parsedVersion = JSON.parse(data);
        dispatch({
          type: SET_SERVER_VERSION_SUCCESS,
          data: parsedVersion.version,
        });
      } catch (e) {
        console.error(e);
      }
    },
    error => {
      console.error(error);
      dispatch({ type: SET_SERVER_VERSION_ERROR, data: null });
    }
  );
};

const fetchServerConfig = () => (dispatch, getState) => {
  const url = Endpoints.serverConfig;
  const options = {
    method: 'GET',
    credentials: globalCookiePolicy,
    headers: getState().tables.dataHeaders,
  };
  dispatch({
    type: FETCHING_SERVER_CONFIG,
  });
  return dispatch(requestAction(url, options)).then(
    data => {
      return dispatch({
        type: SERVER_CONFIG_FETCH_SUCCESS,
        data: data,
      });
    },
    error => {
      return dispatch({
        type: SERVER_CONFIG_FETCH_FAIL,
        data: error,
      });
    }
  );
};

const loadLatestServerVersion = () => (dispatch, getState) => {
  const url =
    Endpoints.updateCheck +
    '?agent=console&version=' +
    getState().main.serverVersion;
  const options = {
    method: 'GET',
    credentials: globalCookiePolicy,
    headers: { 'content-type': 'application/json' },
  };
  return dispatch(requestActionPlain(url, options)).then(
    data => {
      let parsedVersion;
      try {
        parsedVersion = JSON.parse(data);
        dispatch({
          type: SET_LATEST_SERVER_VERSION_SUCCESS,
          data: parsedVersion.latest,
        });
      } catch (e) {
        console.error(e);
      }
    },
    error => {
      console.error(error);
      dispatch({ type: SET_LATEST_SERVER_VERSION_ERROR, data: null });
    }
  );
};

const getHeaders = (header, token, defaultValue = null) => {
  let headers = {};
  switch (header) {
    case 'pat':
      const personalAccessToken = loadPATState();
      headers = {
        ...CONSTANT_HEADERS,
        [globals.patLabel]: `pat ${personalAccessToken}`,
      };
      return headers;
    case 'collabToken':
      headers = {
        ...CONSTANT_HEADERS,
        [globals.collabLabel]: token,
      };
      return headers;
    case 'adminSecret':
      let adminSecret = null;
      if (defaultValue) {
        adminSecret = defaultValue;
      } else {
        if (globals.consoleMode === SERVER_CONSOLE_MODE) {
          adminSecret =
            extendedGlobals.adminSecret ||
            loadAdminSecretState() ||
            globals.adminSecret;
        } else {
          adminSecret = globals.adminSecret;
        }
      }
      headers = {
        ...CONSTANT_HEADERS,
        [`x-hasura-${globals.adminSecretLabel}`]: adminSecret,
      };
      return headers;
    case 'ssoToken':
      headers = {
        ...CONSTANT_HEADERS,
        [globals.ssoLabel]: token,
      };
      return headers;
    default:
      return null;
  }
};

const validateLogin = isInitialLoad => (dispatch, getState) => {
  const url = Endpoints.metadata;
  const { search } = getState().routing.locationBeforeTransitions;
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: getState().tables.dataHeaders,
    body: JSON.stringify({ type: 'export_metadata', args: {} }),
  };
  if (isInitialLoad) {
    return dispatch(requestAction(url, options)).then(data =>
      dispatch({
        type: SET_METADATA,
        data: { ...data, loading: false },
      })
    );
  }
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: LOGIN_IN_PROGRESS, data: false });
      dispatch({ type: LOGIN_ERROR, data: false });
      const parseQueryString = parseQueryParams(search);
      if (
        'redirect_url' in parseQueryString &&
        parseQueryString.redirect_url &&
        parseQueryString.redirect_url !== 'undefined'
      ) {
        dispatch(push(`${globals.urlPrefix}${parseQueryString.redirect_url}`));
      } else {
        dispatch(push(globals.urlPrefix));
      }
      dispatch({
        type: SET_METADATA,
        data: { ...data, loading: false },
      });
      return true;
    },
    error => {
      dispatch({ type: LOGIN_IN_PROGRESS, data: false });
      dispatch({ type: LOGIN_ERROR, data: true });
      dispatch(showErrorNotification('Login Failed', '', error));
      console.error(
        `Failed to validate ${globals.adminSecretLabel} + ${JSON.stringify(
          error
        )}`
      );
      return false;
    }
  );
};

const getTimeDifference = exp => {
  /* Refreshing 30 secs back */
  return (exp - 30) * 1000;
};

const getExpiryDate = exp => {
  const cD = new Date();
  const futureTime = cD.getTime() + exp;
  return new Date(futureTime);
};

const handleSystemSuspendWakeUp = (dispatch, expiryDate) => {
  let lastTime = new Date().getTime();
  console.info('Keeping a tab at the system sleeps/wakeup');
  const pollId = setInterval(() => {
    const currentTime = new Date().getTime();
    if (currentTime > lastTime + 2000 * 2) {
      // Probably just woke up!
      console.info('Console system just woke up, checking the credentials');
      if (expiryDate.getTime() < currentTime) {
        clearInterval(pollId);
        const onlinePoll = setInterval(() => {
          if (!navigator) {
            clearInterval(onlinePoll);
            dispatch(push(`${globals.urlPrefix}login`));
          } else if (navigator && navigator.onLine) {
            clearInterval(onlinePoll);
            dispatch(push(`${globals.urlPrefix}login`));
          }
        }, 2000);
      }
      return pollId;
    }
    lastTime = currentTime;
  }, 2000);
  return pollId;
};

const clearCollaboratorSignInState = () => {
  return (dispatch, getState) => {
    const { headers } = getState().apiexplorer.displayedApi.request;
    try {
      const dispatcherChains = [];
      if (headers && headers.length !== 0) {
        const headerIndex = headers.findIndex(
          element => element.key === globals.collabLabel
        );
        const patIndex = headers.findIndex(
          element => element.key === globals.patLabel
        );
        const ssoIndex = headers.findIndex(
          element => element.key === globals.ssoLabel
        );
        if (headerIndex !== -1) {
          dispatcherChains.push(dispatch(removeRequestHeader(headerIndex)));
        }
        if (patIndex !== -1) {
          dispatcherChains.push(dispatch(removeRequestHeader(patIndex)));
        }
        if (ssoIndex !== -1) {
          dispatcherChains.push(dispatch(removeRequestHeader(ssoIndex)));
        }
      }
      dispatcherChains.push(
        dispatch({ type: UPDATE_DATA_HEADERS, data: { ...CONSTANT_HEADERS } })
      );
      dispatcherChains.push(
        dispatch({
          type: UPDATE_HASURA_DOT_COM_ACCESS,
          data: {},
        })
      );

      Promise.all(dispatcherChains)
        .then(() => {
          removeHeaderFromLS(globals.collabLabel);
          removeHeaderFromLS(globals.patLabel);
          removeHeaderFromLS(globals.ssoLabel);
          clearPATState();
          clearAdminSecretState();
          initLS();
          dispatch(push(`${globals.urlPrefix}/login`));
        })
        .catch(e => {
          throw e;
        });
    } catch (e) {
      dispatch(showErrorNotification('Logout Failed', '', e));
    }
  };
};

const idTokenReceived =
  (idp, data, shouldRedirect = true) =>
  (dispatch, getState) => {
    // set localstorage
    const { id_token: idToken } = data;
    const bearerToken = `IDToken ${idToken}`;
    const updatedDataHeaders = getHeaders('collabToken', bearerToken);
    /* Remove admin-secret if applicable and add new data headers into the LS */
    /* Implement some sort of a timeout which refetches the token
     * from refresh token
     * */
    const { expires_in: expiresIn } = data;
    if (expiresIn > 0) {
      const timeDiff = getTimeDifference(expiresIn);
      const expiryDate = getExpiryDate(timeDiff);
      console.info('Token will be refreshed at', expiryDate);
      const pollId = handleSystemSuspendWakeUp(dispatch, expiryDate);
      setTimeout(() => {
        // This variable should be used to determine whether to redirect the user in the case of fresh OAuthCallback flow
        // Clear the interval before invoking the function again
        clearInterval(pollId);
        dispatch(retrieveByRefreshToken(idp, data.refresh_token))
          .then(resp => {
            dispatch(idTokenReceived(resp, false));
          })
          .catch(err => {
            console.error(err);
            const { routing } = getState();
            const { locationBeforeTransitions } = routing;
            const { pathname, search } = locationBeforeTransitions;
            const redirectUrl = constructRedirectUrl(pathname, search);
            if (redirectUrl) {
              dispatch(
                push({
                  pathname: '/login',
                  search: `?redirect_url=${window.encodeURIComponent(
                    redirectUrl
                  )}`,
                })
              );
              return;
            }
            dispatch(push(`${globals.urlPrefix}/login`));
            return;
          });
      }, timeDiff);
    } else {
      console.error('Unexpected error');
      dispatch(push('/'));
    }

    const decodedToken = decodeToken(idToken) || {};
    const currentHeaders = getState().apiexplorer.displayedApi.request.headers;
    let collabIndex = 1;
    if (currentHeaders) {
      const index = currentHeaders.findIndex(
        f => f.key === globals.collabLabel
      );
      if (index !== -1) {
        collabIndex = index;
      }
    }

    Promise.all([
      dispatch({ type: UPDATE_DATA_HEADERS, data: updatedDataHeaders }),
      ...(globals.isAdminSecretSet
        ? [
            dispatch(
              changeRequestHeader(collabIndex, 'key', globals.collabLabel, true)
            ),
            dispatch(
              changeRequestHeader(collabIndex, 'value', bearerToken, true)
            ),
          ]
        : []),
      dispatch({
        type: UPDATE_HASURA_DOT_COM_ACCESS,
        data: { ...data, tokenInfo: { ...decodedToken } },
      }),
      // dispatch(push('/'))
    ]).then(() => {
      const project = decodedToken.payload?.project;
      dispatch({
        type: FETCHED_LUX_PROJECT_INFO,
        data: {
          id: project.id,
          name: project.name,
          privileges: decodedToken.payload.collaborator_privileges || [],
          metricsFQDN: decodedToken.payload.metrics_fqdn,
          plan_name: project?.plan_name,
          is_enterprise_user: project?.owner?.enterprise_user?.is_active,
          entitlements: project?.entitlements,
        },
      });
      /* Flush to the local storage */
      if (globals.isAdminSecretSet) {
        upsertToLS(globals.collabLabel, bearerToken);
      } else {
        // Set the client name header if doesn't exist
        upsertToLS(CLIENT_NAME_HEADER, CLIENT_NAME_HEADER_VALUE);
        // Remove collaborator token header if exists
        removeHeaderFromLS(globals.collabLabel);
      }
      let redirectFromLS = '';
      if (shouldRedirect) {
        try {
          redirectFromLS = getKeyFromLS('redirectUrl');
        } catch (e) {
          redirectFromLS = '';
        }
        if (redirectFromLS) {
          dispatch(push(`${globals.urlPrefix}${redirectFromLS}`));
        } else {
          dispatch(push(globals.urlPrefix));
        }
        // Not required as the OAUTH key received is assumed to be valid
        // dispatch(validateLogin(false));
      }
    });
  };

// the sso access token is received from OAuth or SAML Idp of the EE customer
// because it doesn't have permission to request resources from lux
// we only store the token into graphiql headers and the local storage
const ssoIdTokenReceived =
  (idp, data, shouldRedirect = true) =>
  async (dispatch, getState) => {
    const {
      access_token: accessToken,
      expires_in: expiresIn,
      refresh_token: refreshToken,
      id_token: idToken,
    } = data;

    // prefer jwt id_token
    const token = idToken || accessToken;
    const bearerToken = `IDToken ${token}`;
    const updatedDataHeaders = getHeaders('ssoToken', bearerToken);

    /* Remove admin-secret if applicable and add new data headers into the LS */
    /* Implement some sort of a timeout which refetches the token
     * from refresh token
     * */
    if (expiresIn > 0) {
      const timeDiff = getTimeDifference(expiresIn);
      const expiryDate = getExpiryDate(timeDiff);
      console.info('Token will be refreshed at', expiryDate);
      const pollId = handleSystemSuspendWakeUp(dispatch, expiryDate);

      const onFailure = () => {
        const { routing } = getState();
        const { locationBeforeTransitions } = routing;
        const { pathname, search } = locationBeforeTransitions;
        const redirectUrl = constructRedirectUrl(pathname, search);
        if (redirectUrl) {
          dispatch(
            push({
              pathname: '/login',
              search: `?redirect_url=${window.encodeURIComponent(redirectUrl)}`,
            })
          );
          return;
        }
        dispatch(push(`${globals.urlPrefix}/login`));
      };

      setTimeout(() => {
        // This variable should be used to determine whether to redirect the user in the case of fresh OAuthCallback flow
        // Clear the interval before invoking the function again
        clearInterval(pollId);

        if (!refreshToken) {
          return onFailure();
        }

        dispatch(retrieveByRefreshToken(idp, refreshToken))
          .then(resp => {
            dispatch(idTokenReceived(resp, false));
          })
          .catch(err => {
            console.error(err);
          });
      }, timeDiff);
    } else {
      console.error('Unexpected error');
      dispatch(push('/'));
    }

    const currentHeaders = getState().apiexplorer.displayedApi.request.headers;
    let authHeaderIndex = 1;
    if (currentHeaders) {
      const index = currentHeaders.findIndex(f => f.key === globals.ssoLabel);
      if (index !== -1) {
        authHeaderIndex = index;
      }
    }

    await Promise.all([
      dispatch({ type: UPDATE_DATA_HEADERS, data: updatedDataHeaders }),
      ...(globals.isAdminSecretSet
        ? [
            dispatch(
              changeRequestHeader(
                authHeaderIndex,
                'key',
                globals.ssoLabel,
                true
              )
            ),
            dispatch(
              changeRequestHeader(authHeaderIndex, 'value', bearerToken, true)
            ),
          ]
        : []),
    ]);

    /* Flush to the local storage */
    if (globals.isAdminSecretSet) {
      upsertToLS(globals.ssoLabel, bearerToken);
    } else {
      // Set the client name header if doesn't exist
      upsertToLS(CLIENT_NAME_HEADER, CLIENT_NAME_HEADER_VALUE);
      // Remove sso token header if exists
      removeHeaderFromLS(globals.ssoLabel);
    }

    // fetch server config to check if the current token has admin role
    const isAdmin = await dispatch(validateLogin(false)).catch(() => false);

    if (!isAdmin) {
      return dispatch(clearCollaboratorSignInState());
    }

    let redirectFromLS = '';
    if (shouldRedirect) {
      try {
        redirectFromLS = getKeyFromLS('redirectUrl');
      } catch (e) {
        redirectFromLS = '';
      }
      if (redirectFromLS) {
        dispatch(push(`${globals.urlPrefix}${redirectFromLS}`));
      } else {
        dispatch(push(globals.urlPrefix));
      }
    }
  };

const loginClicked = () => (dispatch, getState) => {
  // set localstorage
  dispatch({ type: LOGIN_IN_PROGRESS, data: true });
  const adminSecretInput = getState().main.adminSecretInput;
  saveAdminSecretState(adminSecretInput);
  // redirect to / to test the adminSecretInput;
  const updatedDataHeaders = getHeaders('adminSecret');
  Promise.all([
    dispatch({ type: ADMIN_SECRET_ERROR, data: false }),
    dispatch({ type: UPDATE_DATA_HEADERS, data: updatedDataHeaders }),
    dispatch(
      changeRequestHeader(
        1,
        'key',
        `x-hasura-${globals.adminSecretLabel}`,
        true
      )
    ),
    dispatch(changeRequestHeader(1, 'value', adminSecretInput, true)),
    // dispatch(push('/'))
  ]).then(() => {
    /* Flush to local storage */
    upsertToLS(`x-hasura-${globals.adminSecretLabel}`, adminSecretInput);
    // make a sample query. check error code and push to /
    dispatch(validateLogin(false));
  });
};

const patLoginClicked = () => (dispatch, getState) => {
  // set localstorage
  dispatch({ type: LOGIN_IN_PROGRESS, data: true });
  const personalAccessToken = getState().main.personalAccessToken;
  savePATState(personalAccessToken);
  // redirect to / to test the personalAccessToken;
  const updatedDataHeaders = getHeaders('pat');
  Promise.all([
    dispatch({ type: ADMIN_SECRET_ERROR, data: false }),
    dispatch({ type: UPDATE_DATA_HEADERS, data: updatedDataHeaders }),
    ...(globals.isAdminSecretSet
      ? [
          dispatch(changeRequestHeader(1, 'key', globals.patLabel, true)),
          dispatch(
            changeRequestHeader(1, 'value', `pat ${personalAccessToken}`, true)
          ),
        ]
      : []),
    // dispatch(push('/'))
  ]).then(() => {
    /* Flush to local storage */
    if (globals.isAdminSecretSet) {
      upsertToLS(globals.patLabel, `pat ${personalAccessToken}`);
    } else {
      // Set the client name header if doesn't exist
      upsertToLS(CLIENT_NAME_HEADER, CLIENT_NAME_HEADER_VALUE);
      // Remove collaborator token header if exists
      removeHeaderFromLS(globals.patLabel);
    }
    // make a sample query. check error code and push to /
    dispatch(validateLogin(false));
  });
};

export const loadLuxProjectInfo = () => (dispatch, getState) => {
  if (globals.consoleMode !== 'cli' && globals.consoleType !== 'cloud') {
    return Promise.resolve();
  }
  const url = Endpoints.luxDataGraphql;
  const reqOptions = {
    method: 'POST',
    credentials: 'include',
    body: JSON.stringify({
      query: `
        query getLuxProjectInfo($id: uuid!) {
          users {
            id
            email
          }
          projects_by_pk(id: $id) {
            id
            owner {
              id
              email
              enterprise_user {
                is_active
              }
            }
            name
            collaborators {
              collaborator {
                id
                email
              }
              id
              project_collaborator_privileges {
                privilege_slug
              }
            }
            tenant {
              region_info {
                metrics_fqdn
              }
            }
            plan_name
          }
        }
      `,
      variables: {
        id: globals.hasuraCloudProjectId,
      },
    }),
  };

  if (globals.consoleMode === 'cli') {
    reqOptions.headers = {
      ...getState().tables.dataHeaders,
    };
  }

  dispatch({
    type: FETCHING_LUX_PROJECT_INFO,
    data: true,
  });

  dispatch(requestAction(url, reqOptions))
    .then(resp => {
      dispatch({
        type: FETCHING_LUX_PROJECT_INFO,
        data: false,
      });
      if (!resp.data || !resp.data.projects_by_pk) {
        throw new Error(resp.errors[0]?.message);
      }
      const project = resp.data.projects_by_pk;
      const user = resp.data.users[0];
      const isOwner = project.owner.id === user.id;
      const projectInfo = {
        id: project.id,
        name: project.name,
        privileges: isOwner
          ? ['admin', 'graphql_admin', 'view_metrics']
          : (
              project.collaborators.find(c => c.collaborator.id === user.id)
                ?.project_collaborator_privileges || []
            ).map(p => p.privilege_slug),
        metricsFQDN: project.tenant?.region_info?.metrics_fqdn || '',
        plan_name: project?.plan_name,
        is_enterprise_user: project?.owner?.enterprise_user?.is_active,
        entitlements: project?.entitlements,
      };

      dispatch({
        type: FETCHED_LUX_PROJECT_INFO,
        data: projectInfo,
      });
    })
    .catch(e => {
      console.error(e);
      dispatch({
        type: ERROR_FETCHING_LUX_PROJECT_INFO,
        error: e,
      });
    });
};

export const loadLuxProjectEntitlements = () => (dispatch, getState) => {
  if (!isCloudConsole(globals)) {
    return Promise.resolve();
  }

  const url = Endpoints.luxDataGraphql;
  const reqOptions = {
    method: 'POST',
    credentials: 'include',
    body: JSON.stringify({
      query: `
        query getLuxProjectEntitlements($id: uuid!) {
          projects_by_pk(id: $id) {
            entitlements {
              id
              entitlement {
                type
                config_is_enabled
              }
            }
          }
        }
      `,
      variables: {
        id: globals.hasuraCloudProjectId,
      },
    }),
  };

  if (globals.consoleMode === 'cli') {
    reqOptions.headers = {
      ...getState().tables.dataHeaders,
    };
  }

  dispatch({
    type: FETCHING_LUX_PROJECT_ENTITLEMENTS,
    data: true,
  });

  dispatch(requestAction(url, reqOptions))
    .then(resp => {
      dispatch({
        type: FETCHING_LUX_PROJECT_ENTITLEMENTS,
        data: false,
      });
      if (!resp.data || !resp.data.projects_by_pk) {
        console.error(
          'getLuxProjectEntitlements error',
          resp.errors[0]?.message
        );
      }

      const projectEntitlements = {
        entitlements: resp?.data?.projects_by_pk?.entitlements,
      };

      dispatch({
        type: FETCHED_LUX_PROJECT_ENTITLEMENTS,
        data: projectEntitlements,
      });
    })
    .catch(e => {
      console.error(e);
      dispatch({
        type: ERROR_FETCHING_LUX_PROJECT_ENTITLEMENTS,
        error: e,
      });
    });
};

const updateMigrationModeStatus = () => (dispatch, getState) => {
  // make req to hasura cli to update migration mode
  dispatch({ type: UPDATE_MIGRATION_MODE_PROGRESS, data: true });
  const url = Endpoints.hasuractlMigrateSettings;
  const putBody = {
    name: 'migration_mode',
    value: (!getState().main.migrationMode).toString(),
  };
  const options = {
    method: 'PUT',
    credentials: globalCookiePolicy,
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify(putBody),
  };
  return dispatch(requestAction(url, options, UPDATE_MIGRATION_MODE)).then(
    () => {
      // check if migration mode is off and send metadata export
      dispatch({ type: UPDATE_MIGRATION_MODE_PROGRESS, data: false });
      if (!getState().main.migrationMode) {
        // if its off
        const metadataOptions = {
          method: 'GET',
          credentials: globalCookiePolicy,
          headers: { 'content-type': 'application/json' },
        };
        const metadataUrl = `${Endpoints.hasuractlMetadata}?export=true`;
        return dispatch(
          requestAction(
            metadataUrl,
            metadataOptions,
            EXPORT_METADATA_SUCCESS,
            EXPORT_METADATA_ERROR
          )
        );
      }
    }
  );
  // refresh console
};

// default state is the combination of mainState and pro's mainState
const proMainReducer = (state = { ...mainState, ...defaultState }, action) => {
  switch (action.type) {
    case UPDATE_PERSONAL_ACCESS_TOKEN:
      return { ...state, personalAccessToken: action.data };
    case UPDATE_PROJECT_ID:
      return { ...state, projectId: action.data };
    case UPDATE_PROJECT_NAME:
      return { ...state, projectName: action.data };
    case UPDATE_HASURA_DOT_COM_ACCESS:
      return {
        ...state,
        oAuthResponse: {
          ...action.data,
        },
      };
    case SET_METADATA:
      return {
        ...state,
        metadata: {
          loading: false,
          ...action.data,
        },
      };
    case SET_METADATA_LOADING:
      return {
        ...state,
        metadata: {
          loading: action.data,
          ...state.metadata,
        },
      };
    case FETCHING_LUX_PROJECT_INFO:
      return {
        ...state,
        project: {
          loading: action.data,
          ...state.project,
        },
      };
    case FETCHED_LUX_PROJECT_INFO:
      return {
        ...state,
        project: action.data,
      };
    case ERROR_FETCHING_LUX_PROJECT_INFO:
      return {
        ...state,
        project: {
          ...state.project,
          loading: false,
        },
      };
    case FETCHING_LUX_PROJECT_ENTITLEMENTS:
      return {
        ...state,
        project: {
          ...state.project,
          loading: action.data,
        },
      };
    case FETCHED_LUX_PROJECT_ENTITLEMENTS:
      return {
        ...state,
        project: {
          ...state.project,
          loading: false,
        },
        projectEntitlements: action?.data?.entitlements,
      };
    case ERROR_FETCHING_LUX_PROJECT_ENTITLEMENTS:
      return {
        ...state,
        project: {
          ...state.project,
          loading: false,
        },
      };
    default:
      const nextMainState = mainReducer(state, action);
      /*
       * - state object will always be the superset of oss's main state and hence the state is overwrote by the next oss's main state
       * */
      return {
        ...state,
        ...nextMainState,
      };
  }
};

export default proMainReducer;
export {
  HASURACTL_URL_ENV,
  UPDATE_MIGRATION_STATUS_SUCCESS,
  UPDATE_MIGRATION_STATUS_ERROR,
  UPDATE_ADMIN_SECRET_INPUT,
  UPDATE_PERSONAL_ACCESS_TOKEN,
  UPDATE_PROJECT_ID,
  UPDATE_PROJECT_NAME,
  loadMigrationStatus,
  updateMigrationModeStatus,
  loginClicked,
  LOGIN_IN_PROGRESS,
  LOGIN_ERROR,
  validateLogin,
  loadServerVersion,
  fetchServerConfig,
  loadLatestServerVersion,
  featureCompatibilityInit,
  idTokenReceived,
  clearCollaboratorSignInState,
  patLoginClicked,
  getHeaders,
  refetchMetadata,
  setApiLimits,
  ssoIdTokenReceived,
};
