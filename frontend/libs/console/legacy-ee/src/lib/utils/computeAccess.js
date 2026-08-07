import { jwtDecode } from 'jwt-decode';
import { PRIVILEGES } from '../constants';

// jwt-decode@4's jwtDecode returns the claims object directly, whereas the
// previous jsonwebtoken `decode(token, { complete: true })` returned
// `{ header, payload, signature }` (or `null` for an invalid token).
// Consumers (Actions.js, routes.js, Main.js) read `.payload` and rely on a
// falsy return for invalid tokens, so restore that shape here rather than
// changing every call site.
export const decodeToken = idToken => {
  try {
    return {
      header: jwtDecode(idToken, { header: true }),
      payload: jwtDecode(idToken),
    };
  } catch {
    return null;
  }
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
