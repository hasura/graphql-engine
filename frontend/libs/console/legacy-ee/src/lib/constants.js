import * as constants from '@hasura/console-legacy-ce';

export const {
  SERVER_CONSOLE_MODE,
  CLI_CONSOLE_MODE,
  ADMIN_SECRET_HEADER_KEY,
  HASURA_COLLABORATOR_TOKEN,
  HASURA_CLIENT_NAME,
  REDUX_LOCATION_CHANGE_ACTION_TYPE,
} = constants;

export const PRIVILEGES = {
  Admin: 'admin',
  GraphQLAdmin: 'graphql_admin',
  ViewMetrics: 'view_metrics',
  AddCollaborators: 'add_collaborators',
  EventTriggerAdmin: 'event_trigger_admin',
  RemoteSchemaAdmin: 'remote_schema_admin',
  ActionAdmin: 'action_admin',
};

export const CLIENT_NAME_HEADER = 'Hasura-Client-Name';
export const CLIENT_NAME_HEADER_VALUE = 'hasura-console';
export const CONTENT_TYPE_HEADER = 'Content-Type';
export const OAUTH_CALLBACK_URL = '/oauth2/callback';

export const CONSTANT_HEADERS = {
  [CONTENT_TYPE_HEADER]: 'application/json',
  [CLIENT_NAME_HEADER]: CLIENT_NAME_HEADER_VALUE,
};
