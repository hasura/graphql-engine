import { CloudAccessState } from './privileges';

export const restrictedPathsMetadata = {
  '/data': {
    keyInAccessState: 'hasDataAccess',
    replace: '/access-denied',
    errorMessage: 'You do not have access to this tab',
  },
  '/settings': {
    keyInAccessState: 'hasDataAccess',
    replace: '/access-denied',
    errorMessage: 'You do not have access to this tab',
  },
  '/events': {
    keyInAccessState: 'hasEventAccess',
    replace: '/access-denied',
    errorMessage: 'You do not have access to this tab',
  },
  '/remote-schemas': {
    keyInAccessState: 'hasRemoteAccess',
    replace: '/access-denied',
    errorMessage: 'You do not have access to this tab',
  },
  '/api': {
    keyInAccessState: 'hasGraphQLAccess',
    replace: '/access-denied',
    errorMessage: 'You do not have access to this tab',
  },
  '/api-explorer': {
    keyInAccessState: 'hasGraphQLAccess',
    replace: '/access-denied',
    errorMessage: 'You do not have access to this tab',
  },
  '/pro': {
    keyInAccessState: 'hasMetricAccess',
    replace: '/access-denied',
    errorMessage: 'You do not have access to this tab',
  },
  '/actions': {
    keyInAccessState: 'hasActionAccess',
    replace: '/access-denied',
    errorMessage: 'You do not have access to this tab',
  },
};

export const getConsolidatedPath = (
  accessState: CloudAccessState,
  path: string,
  pathPrefix: string,
) => {
  if (restrictedPathsMetadata[pathPrefix]) {
    if (!accessState[restrictedPathsMetadata[pathPrefix].keyInAccessState]) {
      return restrictedPathsMetadata[pathPrefix].replace;
    }
  }

  return path;
};
