export const REGISTER_HEROKU_WEBHOOK = `
  mutation registerHerokuWebhook(
    $herokuAppId: String!
    $projectID: uuid!
    $herokuAppName: String!
  ) {
    herokuRegisterWebhook(
      appID: $herokuAppId
      projectID: $projectID
      appName: $herokuAppName
    ) {
      status
    }
  }
`;

export const CONNECT_HEROKU_APP = `
  mutation connectHerokuApp($herokuAppName: String!, $projectID: uuid!) {
    insert_project_metadata_one(
      object: {
        project_id: $projectID
        heroku_managed_mode: false
        connected_heroku_app_name: $herokuAppName
      }
      on_conflict: {
        constraint: project_metadata_pkey
        update_columns: [connected_heroku_app_name, heroku_managed_mode]
      }
    ) {
      project_id
    }
  }
`;

export const HEROKU_TOKEN_REFRESH = `
  mutation refreshHerokuToken($refreshToken: String!) {
    herokuTokenExchange(payload: { type: refresh, value: $refreshToken }) {
      access_token
      expires_in
      token_type
      refresh_token
    }
  }
`;
