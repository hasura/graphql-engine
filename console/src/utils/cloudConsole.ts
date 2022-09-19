import globals from '@/Globals';
import type { LuxFeature } from '@/Globals';

/*
 * This function returns true only if the current context is Hasura Cloud
 * consoleType === 'cloud' is not enough because
 * consoleType === 'cloud' is also true for Hasura EE
 * */
export function isCloudConsole(g: typeof globals) {
  return g.consoleType === 'cloud' && g.hasuraCloudTenantId;
}

//  This function returns true if the current user has access to a lux feature
export function hasLuxFeatureAccess(g: typeof globals, feature: LuxFeature) {
  return (globals.allowedLuxFeatures || []).includes(feature);
}
