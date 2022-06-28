import Endpoints from '../../../../../Endpoints';
import { OnboardingSampleDBCohortConfig } from './ReduxState';

// this can later be pulled out as a common lux client for console codebase
const newLuxClient = () => {
  const fetchWithOptions = (query: string, variables: any): Promise<any> => {
    return fetch(Endpoints.luxDataGraphql, {
      method: 'POST',
      headers: {
        'content-type': 'application/json',
      },
      credentials: 'include',
      body: JSON.stringify({
        query,
        variables: variables || {},
      }),
    })
      .then(response => {
        return response.json();
      })
      .catch(() => {
        return {
          errors: [
            {
              message: 'unexpected http exception',
            },
          ],
        };
      });
  };

  return {
    query: fetchWithOptions,
  };
};

const luxClient = newLuxClient();

export const fetchSampleDBCohortConfig = () => {
  const query = `
    query {
      onboarding_sample_db_config {
      	databaseUrl: database_url
      	status
      }
    }
  `;
  return luxClient
    .query(query, {})
    .then(r => {
      if (r.errors) {
        throw new Error(r.errors[0]?.message || 'unexpected');
      }
      if (!r.data.onboarding_sample_db_config.length) {
        return null;
      }
      return r.data
        .onboarding_sample_db_config[0] as OnboardingSampleDBCohortConfig;
    })
    .catch(e => {
      throw e;
    });
};

export const trackTryButtonClickEvent = (projectId: string) => {
  const query = `
    mutation trackTryBUttonClickEvent ($projectId: uuid!){
      trackOnboardingSampleDbCohortActivity (
        error_message: "",
        event: "event_try_button"
        project_id: $projectId
        status: ""
      ) {
        status
      }
    }
  `;
  return luxClient
    .query(query, { projectId })
    .then(r => {
      if (r.errors) {
        console.error(
          'failed to track onboarding cohort activity: ',
          r.errors[0]?.message || 'unexpected'
        );
      }
      console.log(r);
    })
    .catch(e => {
      console.error(
        'unexpectedly failed to track onboarding cohort activity: ',
        e.message
      );
    });
};

export const trackConnectButtonClickEvent = (projectId: string) => {
  const query = `
    mutation trackConnectButtonClickEvent($projectId: uuid!) {
      trackOnboardingSampleDbCohortActivity (
        error_message: "",
        event: "event_connect_button"
        project_id: $projectId
        status: ""
      ) {
        status
      }
    }
  `;
  return luxClient
    .query(query, { projectId })
    .then(r => {
      if (r.errors) {
        console.error(
          'failed to track onboarding cohort activity: ',
          r.errors[0]?.message || 'unexpected'
        );
      }
    })
    .catch(e => {
      console.error(
        'unexpectedly failed to track onboarding cohort activity: ',
        e.message
      );
    });
};

export const trackDBConnectionStatusEvent = (
  projectId: string,
  status: 'success' | 'error',
  errorMsg?: string
) => {
  const query = `
    mutation trackDBConnectionStatusEvent($projectId: uuid!, $errorMsg: String!, $status: String!) {
      trackOnboardingSampleDbCohortActivity (
        error_message: $errorMsg,
        event: "event_connection_status"
        project_id: $projectId
        status: $status
      ) {
        status
      }
    }
  `;
  return luxClient
    .query(query, { projectId, status, errorMsg: errorMsg || '' })
    .then(r => {
      if (r.errors) {
        console.error(
          'failed to track onboarding cohort activity: ',
          r.errors[0]?.message || 'unexpected'
        );
      }
    })
    .catch(e => {
      console.error(
        'unexpectedly failed to track onboarding cohort activity: ',
        e.message
      );
    });
};

export const trackAnotherDBConnection = (projectId: string) => {
  const query = `
    mutation trackAnotherDBConnection($projectId: uuid!) {
      trackOnboardingSampleDbCohortActivity (
        error_message: "" 
        event: "event_connected_another_db"
        project_id: $projectId
        status: ""
      ) {
        status
      }
    }
  `;
  return luxClient
    .query(query, { projectId })
    .then(r => {
      if (r.errors) {
        console.error(
          'failed to track onboarding cohort activity: ',
          r.errors[0]?.message || 'unexpected'
        );
      }
    })
    .catch(e => {
      console.error(
        'unexpectedly failed to track onboarding cohort activity: ',
        e.message
      );
    });
};

export const trackLandingOnConnectDBForm = (projectId: string) => {
  const query = `
    mutation trackAnotherDBConnection($projectId: uuid!) {
      trackOnboardingSampleDbCohortActivity (
        error_message: "" 
        event: "event_land_connect_db_form"
        project_id: $projectId
        status: ""
      ) {
        status
      }
    }
  `;
  return luxClient
    .query(query, { projectId })
    .then(r => {
      if (r.errors) {
        console.error(
          'failed to track onboarding cohort activity: ',
          r.errors[0]?.message || 'unexpected'
        );
      }
    })
    .catch(e => {
      console.error(
        'unexpectedly failed to track onboarding cohort activity: ',
        e.message
      );
    });
};
