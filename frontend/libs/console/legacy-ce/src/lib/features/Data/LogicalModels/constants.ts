export const LOGICAL_MODEL_CREATE_SUCCESS =
  'Successfully tracked Logical Model';
export const LOGICAL_MODEL_CREATE_ERROR = 'Unable to track Logical Model';
export const STORED_PROCEDURE_TRACK_SUCCESS =
  'Successfully tracked Stored Procedure';
export const STORED_PROCEDURE_TRACK_ERROR = 'Unable to track Stored Procedure';
export const STORED_PROCEDURE_UNTRACK_SUCCESS =
  'Successfully untracked Stored Procedure';
export const STORED_PROCEDURE_UNTRACK_ERROR =
  'Unable to untrack Stored Procedure';

export const NATIVE_QUERY_ROUTES = {
  '/data/native-queries': {
    title: 'Native Queries',
    subtitle: 'Access more queries and operators through SQL on your database',
  },
  '/data/native-queries/create': {
    title: 'Create Native Query',
    subtitle: 'Access more queries and operators through SQL on your database',
  },
  '/data/native-queries/{{source}}/{{name}}': {
    title: '{{name}}',
    subtitle: 'Access more queries and operators through SQL on your database',
  },
  '/data/native-queries/logical-models': {
    title: 'Logical Models',
    subtitle:
      'Creating Logical Models in advance can help generate Native Queries faster',
  },
  '/data/native-queries/logical-models/create': {
    title: 'Logical Models',
    subtitle:
      'Creating Logical Models in advance can help generate Native Queries faster',
  },
  '/data/native-queries/logical-models/{{source}}/{{name}}': {
    title: '{{name}}',
    subtitle:
      'Creating Logical Models in advance can help generate Native Queries faster',
  },
  '/data/native-queries/stored-procedures': {
    title: 'Stored Procedures',
    subtitle: 'Add support for stored procedures on SQL over a GraphQL API',
  },
  '/data/native-queries/stored-procedures/track': {
    title: 'Track Stored Procedure',
    subtitle: 'Expose your stored SQL procedures via the GraphQL API',
  },
  '/data/native-queries/stored-procedures/{{source}}/{{name}}': {
    title: 'Track Stored Procedure',
    subtitle: 'Expose your stored SQL procedures via the GraphQL API',
  },
  '/data/native-queries/logical-models/{{source}}/{{name}}/permissions': {
    title: 'Logical Models Permissions',
    subtitle:
      'Add permissions to your Logical Models to control access to your data',
  },
};
