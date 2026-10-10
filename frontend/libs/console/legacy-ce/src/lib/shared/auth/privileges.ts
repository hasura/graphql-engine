export type Privilege =
  | 'admin'
  | 'graphql_admin'
  | 'view_metrics'
  | 'add_collaborators'
  | 'event_trigger_admin'
  | 'remote_schema_admin'
  | 'action_admin';

export const defaultAccessState = {
  hasDataAccess: true,
  hasGraphQLAccess: true,
  hasEventAccess: true,
  hasRemoteAccess: true,
  hasMetricAccess: false,
  hasActionAccess: true,
};

export type CloudAccessState = typeof defaultAccessState;

export const hasAdminAccess = (privileges: string[] | undefined) =>
  Boolean(privileges?.includes('admin'));

export const hasDataAccess = (privileges: string[] | undefined) =>
  !privileges?.length || privileges.includes('admin');

export const hasGraphQLAccess = (privileges: string[] | undefined) =>
  !privileges?.length ||
  privileges.includes('admin') ||
  privileges.includes('graphql_admin');

export const hasEventAccess = (privileges: string[] | undefined) =>
  !privileges?.length ||
  privileges.includes('admin') ||
  privileges.includes('event_trigger_admin');

export const hasRemoteAccess = (privileges: string[] | undefined) =>
  !privileges?.length ||
  privileges.includes('admin') ||
  privileges.includes('remote_schema_admin');

export const hasActionAccess = (privileges: string[] | undefined) =>
  !privileges?.length ||
  privileges.includes('admin') ||
  privileges.includes('action_admin');

export const hasMetricAccess = (privileges: string[] | undefined) =>
  !privileges?.length ||
  privileges.includes('admin') ||
  privileges.includes('view_metrics');

export const checkAccess = (
  privileges: string[] | undefined,
): CloudAccessState => {
  return {
    hasDataAccess: hasDataAccess(privileges),
    hasGraphQLAccess: hasGraphQLAccess(privileges),
    hasEventAccess: hasEventAccess(privileges),
    hasRemoteAccess: hasRemoteAccess(privileges),
    hasActionAccess: hasActionAccess(privileges),
    hasMetricAccess: hasMetricAccess(privileges),
  };
};
