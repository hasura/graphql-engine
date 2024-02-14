export const LOGICAL_MODEL_CREATE_SUCCESS =
  'Successfully created Logical Model';
export const LOGICAL_MODEL_EDIT_SUCCESS = 'Successfully edited Logical Model';
export const LOGICAL_MODEL_EDIT_ERROR = 'Unable to edit Logical Model';
export const LOGICAL_MODEL_CREATE_ERROR = 'Unable to track Logical Model';
export const STORED_PROCEDURE_TRACK_SUCCESS =
  'Successfully tracked Stored Procedure';
export const STORED_PROCEDURE_TRACK_ERROR = 'Unable to track Stored Procedure';
export const STORED_PROCEDURE_UNTRACK_SUCCESS =
  'Successfully untracked Stored Procedure';
export const STORED_PROCEDURE_UNTRACK_ERROR =
  'Unable to untrack Stored Procedure';
export const COLLECTION_TRACK_TRACK_ERROR = 'Unable to track Collection';

export const Routes = {
  NativeQueries: '/data/native-queries',
  CreateNativeQuery: '/data/native-queries/create',
  EditNativeQuery: '/data/native-queries/{{source}}/{{name}}/{{tab}}',
  LogicalModels: '/data/native-queries/logical-models',
  CreateLogicalModels: '/data/native-queries/logical-models/create',
  EditLogicalModel:
    '/data/native-queries/logical-models/{{source}}/{{name}}/{{tab}}',
  LogicalModelsPermissions:
    '/data/native-queries/logical-models/{{source}}/{{name}}/permissions',
  StoredProcedures: '/data/native-queries/stored-procedures',
  TrackStoredProcedure: '/data/native-queries/stored-procedures/track',
  StoredProcedureDetail:
    '/data/native-queries/stored-procedures/{{source}}/{{name}}',
} as const;

export const NATIVE_QUERY_ROUTE_DETAIL: Record<
  string,
  { title: string; subtitle: string; docLink?: string }
> = {
  '/data/native-queries': {
    title: 'Native Queries',
    subtitle: 'Access more queries and operators through SQL on your database',
    docLink:
      'https://hasura.io/docs/latest/schema/ms-sql-server/logical-models/native-queries/',
  },
  [Routes.CreateNativeQuery]: {
    title: 'Create Native Query',
    subtitle: 'Access more queries and operators through SQL on your database',
  },
  [Routes.EditNativeQuery]: {
    title: '{{name}}',
    subtitle: '',
  },
  [Routes.LogicalModels]: {
    title: 'Logical Models',
    subtitle:
      'Creating Logical Models in advance can help generate Native Queries faster',
    docLink:
      'https://hasura.io/docs/latest/schema/ms-sql-server/logical-models/index/',
  },
  [Routes.CreateLogicalModels]: {
    title: 'Logical Models',
    subtitle:
      'Creating Logical Models in advance can help generate Native Queries faster',
    docLink:
      'https://hasura.io/docs/latest/schema/ms-sql-server/logical-models/native-queries/#2-create-a-native-query',
  },
  [Routes.EditLogicalModel]: {
    title: '{{name}}',
    subtitle: '',
  },
  [Routes.StoredProcedures]: {
    title: 'Stored Procedures',
    subtitle: 'Add support for stored procedures on SQL over a GraphQL API',
    docLink:
      'https://hasura.io/docs/latest/schema/ms-sql-server/logical-models/stored-procedures/',
  },
  [Routes.TrackStoredProcedure]: {
    title: 'Track Stored Procedure',
    subtitle: 'Expose your stored SQL procedures via the GraphQL API',
  },
  [Routes.StoredProcedureDetail]: {
    title: 'Track Stored Procedure',
    subtitle: 'Expose your stored SQL procedures via the GraphQL API',
  },
  [Routes.LogicalModelsPermissions]: {
    title: 'Logical Models Permissions',
    subtitle:
      'Add permissions to your Logical Models to control access to your data',
  },
};
