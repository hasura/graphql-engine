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
    experiment activity
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

export const fetchDatabaseLatencyJobId = gql(`
  mutation CheckDBLatency (
    $project_id: uuid!
  ) {
    checkDBLatency (
      project_id: $project_id
    ) {
      db_latency_job_id
    }
  }
`);

export const fetchInfoFromJobId = gql(`
query ($id: uuid!) {
  jobs_by_pk(id: $id) {
    id
    status
    tasks {
      id
      name
      task_events {
        id
        event_type
        public_event_data
        error
      }
    }
  }
}
`);

export const insertInfoIntoDBLatencyQuery = gql(`
mutation (
  $jobId: uuid!,
  $projectId: uuid!,
  $isLatencyDisplayed: Boolean!,
  $datasDifferenceInMilliseconds: Int!
) {
  insert_db_latency_one(object: {
    job_id: $jobId,
    is_latency_displayed: $isLatencyDisplayed,
    project_id: $projectId,
    console_check_duration: $datasDifferenceInMilliseconds
  }) {
    id
  }
}
`);

export const updateUserClickedChangeProjectRegion = gql(`
mutation ($rowId: uuid!, $isChangeRegionClicked: Boolean!) {
  update_db_latency(where: {id: {_eq: $rowId}}, _set: {is_change_region_clicked: $isChangeRegionClicked}) {
    affected_rows
    returning {
      id
      is_change_region_clicked
    }
  }
}
`);

/**
 * GraphQl subscription to subscribe to one_click_deployment_state_log table
 */
export const FETCH_ONE_CLICK_DEPLOYMENT_STATE_LOG_SUBSCRIPTION = gql(`
subscription fetchOneClickDeploymentStateLogSubscription ($id: bigint!) {
  one_click_deployment_by_pk(id: $id) {
    id
    one_click_deployment_state_logs (order_by: { created_at: asc }) {
      id
      additional_info
      from_state
      to_state
    }
  }
}
`);

/**
 * GraphQl mutation to trigger one click deployment
 */
export const TRIGGER_ONE_CLICK_DEPLOYMENT = gql(`
mutation triggerOneClickDeployment ($projectId: uuid!) {
  triggerOneClickDeployment (project_id: $projectId) {
    message
    status
  }
}
`);
