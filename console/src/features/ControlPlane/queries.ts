import { parse as gql } from 'graphql';

/**
 * GraphQl query to fetch all growth experiments data related to the current user.
 */
export const FETCH_ALL_EXPERIMENTS_DATA = gql(`
query fetchAllExperimentsData {
  experiments_config {
    experiment
    metadata
    status
  }
  experiments_cohort {
    experiment
    activity
  }
}
`);

/**
 * GraphQl query to get the env vars of a project
 */
export const GET_TENANT_ENV = gql(`
query getTenantEnv($tenantId: uuid!) {
  getTenantEnv: getTenantEnv(tenantId: $tenantId) {
    hash
    envVars
  }
}
`);

/**
 * GraphQL mutation to refresh and get the Heroku session of the current user
 */
export const GET_HEROKU_SESSION = gql(`
	mutation {
		getHerokuSession {
			access_token
			refresh_token
			expires_in
			token_type
		}
	}
`);

/**
 * GraphQl mutation to track the activity of users in experiments cohort
 */
export const TRACK_EXPERIMENTS_COHORT_ACTIVITY = gql(`
mutation trackExperimentsCohortActivity ($projectId: uuid!, $experimentId: String!, $kind: String! $error_code: String) {
  trackExperimentsCohortActivity(experiment: $experimentId, payload: {kind: $kind, project_id: $projectId, error_code: $error_code}) {
    status
  }
}
`);

/**
 * GraphQl mutation to update the env vars of a project
 */
export const UPDATE_TENANT_ENV = gql(`
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
`);

/**
 * GraphQL mutation to create a Neon database
 */
export const NEON_CREATE_DATABASE_QUERY = gql(`
 mutation neonCreateDatabase ($projectId:uuid!) {
    neonCreateDatabase (projectId: $projectId) {
      databaseUrl
      email
      envVar
      isAuthenticated
    }
  }
`);

/**
 * GraphQL mutation to exchange Neon OAuth token
 */
export const NEON_TOKEN_EXCHANGE_QUERY = gql(`
  mutation neonTokenExchange (
    $code: String!
    $state: String!
    $projectId: uuid!
  ) {
    neonExchangeOAuthToken (
      code: $code
      state: $state
      projectId: $projectId
    ) {
      accessToken
      email
    }
  }
`);
