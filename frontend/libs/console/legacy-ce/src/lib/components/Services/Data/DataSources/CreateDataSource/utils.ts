/* eslint no-loop-func: 0 */ // --> OFF

import Globals from '../../../../../Globals';
import Endpoints from '../../../../../Endpoints';

export const generateRandomString = (stringLength = 16) => {
  const allChars =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  let str = '';

  for (let i = 0; i < stringLength; i++) {
    const randomNum = Math.floor(Math.random() * allChars.length);
    str += allChars.charAt(randomNum);
  }
  return str;
};

export const getEnvVars = () => {
  const tenantId = Globals.hasuraCloudTenantId;
  const query = `
    query getTenantEnv($tenantId: uuid!) {
      getTenantEnv: getTenantEnv(tenantId: $tenantId) {
        hash
        envVars
      }
    }
  `;
  const variables = {
    tenantId,
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
      return r.json().then(response => {
        if (response.errors) {
          throw new Error(response.errors[0]?.message);
        }
        return response;
      });
    })
    .catch(e => {
      throw e;
    });
};

export const updateEnvVars = (
  currentHash: string,
  envs: { key: any; value: any }[]
) => {
  const tenantId = Globals.hasuraCloudTenantId;
  const query = `
    mutation updateTenant(
      $tenantId: uuid!
      $currentHash: String!
      $envs: [UpdateEnvObject!]!
    ) {
      updateTenantEnv(
        currentHash: $currentHash
        tenantId: $tenantId
        envs: $envs
      ) {
        hash
        envVars
      }
    }
  `;
  const variables = {
    tenantId,
    currentHash,
    envs,
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
      return r.json().then(response => {
        if (response.errors) {
          throw new Error(response.errors[0]?.message);
        } else {
          return response.data.updateTenantEnv;
        }
      });
    })
    .catch(e => {
      throw e;
    });
};

export const getAvailableEnvVar = (envVars: Record<string, any>) => {
  const newEnvVarName = 'PG_DATABASE_URL';
  let suffix = 0;
  while (
    Object.keys(envVars).some(e => e === `${newEnvVarName}${suffix || ''}`) ||
    (envVars.environment &&
      Object.keys(envVars.environment).some(
        e => e === `${newEnvVarName}${suffix || ''}`
      ))
  ) {
    suffix++;
  }
  return `${newEnvVarName}${suffix || ''}`;
};

export const setDBURLInEnvVars = (dbURL: string) => {
  return getEnvVars()
    .then(res => {
      const { hash, envVars } = res.data.getTenantEnv;
      const emptyEnvVar = getAvailableEnvVar(envVars);
      return updateEnvVars(hash, [
        {
          key: emptyEnvVar,
          value: dbURL,
        },
      ]).then(() => {
        return { envVar: emptyEnvVar, oldConfigHash: hash };
      });
    })
    .catch(e => {
      throw e;
    });
};

export const getProjectHealth = () => {
  const healthEndpoint = `${Globals.dataApiUrl}/healthz`;
  return fetch(healthEndpoint, {
    method: 'GET',
    headers: {
      'content-type': 'application/json',
    },
    credentials: 'include',
  })
    .then(health => {
      return health.ok;
    })
    .catch(e => {
      throw e;
    });
};

export const verifyProjectHealthAndConnectDataSource = (
  successCallback: VoidFunction,
  errorCallback: VoidFunction,
  retryCount = 0
) => {
  if (retryCount === 10) {
    errorCallback();
    return;
  }
  getProjectHealth()
    .then(() => {
      successCallback();
    })
    .catch(() => {
      setTimeout(() => {
        verifyProjectHealthAndConnectDataSource(
          successCallback,
          errorCallback,
          retryCount + 1
        );
      }, 1500);
    });
};
