import globals from '../Globals';
import type { LuxFeature } from '../Globals';

/*
 * This function returns true only if the current context is Hasura Cloud
 * consoleType === 'cloud' is not enough because
 * consoleType === 'cloud' is also true for Hasura EE
 * */
export function isCloudConsole(g: typeof globals) {
  return !!g.hasuraCloudTenantId && g.consoleType === 'cloud';
}

export function isEECloud(g: typeof globals) {
  return !g.hasuraCloudTenantId && g.consoleType === 'cloud';
}

//  This function returns true if the current user has access to a lux feature
export function hasLuxFeatureAccess(g: typeof globals, feature: LuxFeature) {
  return (globals.allowedLuxFeatures || []).includes(feature);
}

export function getProjectId(g: typeof globals) {
  return isCloudConsole(g) ? g.hasuraCloudProjectId : undefined;
}

export function getTenantId(g: typeof globals) {
  return isCloudConsole(g) ? g.hasuraCloudTenantId : undefined;
}
