import { type HasuraPlan } from './store';
import { type Compatibility, type ElaboratingMatch } from './isFeatureEnabled';

type CheckPassed = boolean;

// ------------------------------------------------------------
// PLAN CHECKERS
// ------------------------------------------------------------
export function isRunningCePlan(currentState: { hasuraPlan: HasuraPlan }) {
  return currentState.hasuraPlan.name === 'ce';
}

export function isRunningEeWithoutLicensePlan(currentState: {
  hasuraPlan: HasuraPlan;
}) {
  return (
    currentState.hasuraPlan.name === 'ee' &&
    (currentState.hasuraPlan.license.status === 'missing' ||
      currentState.hasuraPlan.license.status === 'deactivated' ||
      currentState.hasuraPlan.license.status === 'expired' ||
      currentState.hasuraPlan.license.status === 'licenseApiError' ||
      currentState.hasuraPlan.license.status === 'unknown')
  );
}

export function isRunningEeWithLicensePlan(currentState: {
  hasuraPlan: HasuraPlan;
}) {
  return (
    currentState.hasuraPlan.name === 'ee' &&
    (currentState.hasuraPlan.license.status === 'active' ||
      currentState.hasuraPlan.license.status === 'grace')
  );
}

// ------------------------------------------------------------
// COMPATIBILITY CHECKERS
// ------------------------------------------------------------
export function checkCe(params: {
  compatibility: Compatibility;
  isRunningCe: boolean;
  mutableDoMatch: ElaboratingMatch;
  mutableDoNotMatch: ElaboratingMatch;
}): CheckPassed {
  const { compatibility, isRunningCe, mutableDoMatch, mutableDoNotMatch } =
    params;

  if (compatibility.ce === 'enabled') {
    if (isRunningCe) mutableDoMatch.ce = true;
    else mutableDoNotMatch.ce = true;
  }

  return !!mutableDoMatch.ce;
}

export function checkEe(params: {
  compatibility: Compatibility;
  isRunningEeWithoutLicense: boolean;
  isRunningEeWithLicense: boolean;
  mutableDoMatch: ElaboratingMatch;
  mutableDoNotMatch: ElaboratingMatch;
}): CheckPassed {
  const {
    compatibility,
    isRunningEeWithoutLicense,
    isRunningEeWithLicense,
    mutableDoMatch,
    mutableDoNotMatch,
  } = params;

  let passEeCheck = false;

  if ('ee' in compatibility) {
    // These two objects will be later removed if remain empty
    mutableDoMatch.ee = {};
    mutableDoNotMatch.ee = {};

    if (compatibility.ee.withoutLicense === 'enabled') {
      if (isRunningEeWithoutLicense) {
        mutableDoMatch.ee.withoutLicense = true;
      } else {
        mutableDoNotMatch.ee.withoutLicense = true;
      }
    }

    if (compatibility.ee.withLicense === 'enabled') {
      if (isRunningEeWithLicense) {
        mutableDoMatch.ee.withLicense = true;
      } else {
        mutableDoNotMatch.ee.withLicense = true;
      }
    }

    passEeCheck =
      mutableDoMatch.ee.withoutLicense === true ||
      mutableDoMatch.ee.withLicense === true;

    // Sub-objects not containing `enabled` options must be removed to avoid mismatching with the
    // types
    if (Object.keys(mutableDoMatch.ee).length === 0) delete mutableDoMatch.ee;
    if (Object.keys(mutableDoNotMatch.ee).length === 0)
      delete mutableDoNotMatch.ee;
  }

  return passEeCheck;
}
