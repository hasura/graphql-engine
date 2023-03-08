import jwt from 'jsonwebtoken';
import { PRIVILEGES } from '../constants';

export const decodeToken = idToken => {
  const decoded = jwt.decode(idToken, { complete: true });
  return decoded;
};

const isIn = (privileges, item) => privileges.indexOf(item) !== -1;

export const defaultAccessState = {
  hasDataAccess: true,
  hasGraphQLAccess: true,
  hasEventAccess: true,
  hasRemoteAccess: true,
  hasMetricAccess: true,
  hasActionAccess: true,
};

export const checkAccess = privileges => {
  if (!privileges) {
    return { ...defaultAccessState };
  }
  return {
    hasDataAccess: isIn(privileges, PRIVILEGES.Admin),
    hasGraphQLAccess:
      isIn(privileges, PRIVILEGES.GraphQLAdmin) ||
      isIn(privileges, PRIVILEGES.Admin),
    hasEventAccess:
      isIn(privileges, PRIVILEGES.Admin) ||
      isIn(privileges, PRIVILEGES.EventTriggerAdmin),
    hasRemoteAccess:
      isIn(privileges, PRIVILEGES.Admin) ||
      isIn(privileges, PRIVILEGES.RemoteSchemaAdmin),
    hasActionAccess:
      isIn(privileges, PRIVILEGES.Admin) ||
      isIn(privileges, PRIVILEGES.ActionAdmin),
    hasMetricAccess:
      isIn(privileges, PRIVILEGES.Admin) ||
      isIn(privileges, PRIVILEGES.ViewMetrics),
  };
};
