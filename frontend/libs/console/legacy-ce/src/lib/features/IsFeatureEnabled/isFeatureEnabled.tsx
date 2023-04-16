import type { ReactElement } from 'react';
import type { PartialDeep } from 'type-fest';

import type { Feature } from './features';
import { type HasuraPlan, useHasuraPlan } from './store';
import type { ConditionalPickDeepCompatibilityProperties } from './types-utils';

import { features } from './features';
import {
  checkCe,
  checkEe,
  isRunningCePlan,
  isRunningEeWithLicensePlan,
  isRunningEeWithoutLicensePlan,
} from './checkers';

// --------------------------------------------------
// TYPES
// --------------------------------------------------

type EnabledOrNot = 'enabled' | 'disabled';

export type Compatibility = {
  ce: EnabledOrNot;

  /**
   * Please note that the EE plan has been also internally called:
   * - "EE Lite
   * - Pro Lite
   * - EE Trial (EE Trial is a temporary state of EE, when the customers are using a Trial license.
   * Use `ee` and specify if the feature needs a license or not)
   */
  ee: {
    /**
     * If set to 'enabled', the feature is enabled when there is an active (trial, grace, or paid)
     * license.
     */
    withLicense: EnabledOrNot;
    /**
     * If set to 'enabled', the feature is enabled when there is not a license or when the license
     * is expired.
     */
    withoutLicense: EnabledOrNot;
  };

  // --------------------------------------------------
  // UNSUPPORTED PLANS YET
  /**
   * The Cloud plan is not supported yet.
   */
  cloud?: never;
  /**
   * The EE Classic plan is not supported yet.
   *
   * Please note that EE Classic has been internally called "Pro" for a long time.
   */
  eeClassic?: never;

  // --------------------------------------------------
  /**
   * The `unknown` state exits only for async reasons (the server type is retrieved form the
   * /v1/version) and cannot be used to identify if a feature is enabled or not.
   * Theoretically, if the plan is unknown, the Console should not show anything to the customers.
   */
  unknown?: never;
};

/**
 * Include all the properties (top level and deep levels) that are specified as enabled, but with the
 * type `true`.
 *
 * @example
 * type CeOnly = Match<{ce: 'enabled', ee: 'disabled'}>
 * //   ^? {ce: true}
 * type CeAndEe = Match<{ce: 'enabled', ee: 'enabled'}>
 * //   ^? {ce: true, ee: true}
 * type eeTrialOnly = Match<{ce: 'disabled', ee: {licenseType: {trial: 'enabled', paid: 'disabled' }}}>
 * //   ^? {ee: {licenseType: {trial: true}}}
 * TODO: after the completion of the API (when ALL the environments are managed) check if this
 * variable shape management is still needed or not
 */
export type Match<PASSED_COMPATIBILITY extends Compatibility> =
  ConditionalPickDeepCompatibilityProperties<PASSED_COMPATIBILITY, 'enabled'>;

/**
 * An internal type used to add as much type safety as possible to the checkCompatibility function.
 * The final type received by the consumers will include ONLY the properties that are originally
 * passed as 'enabled' in the compatibility object.
 */
export type ElaboratingMatch = PartialDeep<{
  ce: true;
  ee: {
    withLicense: true;
    withoutLicense: true;
  };
}>;

type CompatibilityCheckResult<COMPATIBILITY extends Compatibility> = {
  status: 'enabled' | 'disabled';

  doMatch: PartialDeep<Match<COMPATIBILITY>>;
  doNotMatch: PartialDeep<Match<COMPATIBILITY>>;

  current: {
    hasuraPlan: HasuraPlan;
  };
};

// --------------------------------------------------
// CORE
// --------------------------------------------------

/**
 * Check if a compatibility object matches the current state or not.
 *
 * The returned `doMatch` and `doNotMatch` objects only contains the properties that are
 * marked has `enabled` in the compatibility object (ex. `{ce:'enabled'}` will result in
 * doMatch/doNotMatch having only the `ce` property in them).
 *
 * The main goal of this function is to completely hide all the messy, boring, and error-prone logic
 * of checking if a feature is enabled or not in all the existing combinations of plans/modes/server
 * types, console types, etc.
 */
export function checkCompatibility<PASSED_COMPATIBILITY extends Compatibility>(
  compatibility: PASSED_COMPATIBILITY,
  currentState: {
    hasuraPlan: HasuraPlan;
  }
): CompatibilityCheckResult<PASSED_COMPATIBILITY> {
  const doMatch: ElaboratingMatch = {};
  const doNotMatch: ElaboratingMatch = {};

  const passCeCheck = checkCe({
    compatibility,
    mutableDoMatch: doMatch,
    mutableDoNotMatch: doNotMatch,
    isRunningCe: isRunningCePlan(currentState),
  });

  const passEeCheck = checkEe({
    compatibility,
    mutableDoMatch: doMatch,
    mutableDoNotMatch: doNotMatch,
    isRunningEeWithLicense: isRunningEeWithLicensePlan(currentState),
    isRunningEeWithoutLicense: isRunningEeWithoutLicensePlan(currentState),
  });

  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------

  // TODO: Notes for when the  server/cli mode will be implemented: a feature compatible with the
  // mode but not enabled in one of the Hasura plans must then not be enabled!

  if (passCeCheck || passEeCheck) {
    return {
      status: 'enabled',
      // @ts-expect-error doMatch cannot be typed correctly inside this function because it
      // depends on the literal object passed in the function call and cannot be determined here
      doMatch,
      // @ts-expect-error doNotMatch cannot be typed correctly inside this function because it
      // depends on the literal object passed in the function call and cannot be determined here
      doNotMatch,

      current: currentState,
    };
  }

  return {
    status: 'disabled',

    // @ts-expect-error doMatch cannot be typed correctly inside this function because it
    // depends on the literal object passed in the function call and cannot be determined here
    doMatch,
    // @ts-expect-error doMatch cannot be typed correctly inside this function because it
    // depends on the literal object passed in the function call and cannot be determined here
    doNotMatch,

    current: currentState,
  };
}

// --------------------------------------------------
// REACT APIs
// --------------------------------------------------

/**
 * Allow to check if a feature is enabled or not, returning an object that contains all the details
 * about the compatibilities and incompatibilities with the current Hasura plan the Console is running.
 *
 * @example Simplest usage
 * function Prometheus() {
 *   const { status } = useIsFeatureEnabled('prometheus');
 *
 *   if(status === 'disabled') return null
 *
 *   return <div>Enjoy Prometheus!</div>
 * }
 *
 * @example Advanced usage: render something different when the feature is disabled
 * function Prometheus() {
 *   const {
 *     status,
 *     doNotMatch,
 *     current: { hasuraPlan }
 *   } = useIsFeatureEnabled('prometheus');
 *
 *   if(status === 'disabled') {
 *     if (doNotMatch.ee) {
 *       if(hasuraPlan.type === 'ce') {
 *         return <div>Try EE Lite and give all the paid feature a try for free!</div>
 *       }
 *
 *       return <div>Prometheus is enabled for EE Lite only</div>
 *     }
 *   }
 *
 *   return <div>Enjoy Prometheus!</div>
 * }
 */
export function useIsFeatureEnabled<FEATURE extends Feature>(
  featureName: FEATURE
) {
  const hasuraPlan = useHasuraPlan();
  const compatibility = features[featureName];

  return checkCompatibility(compatibility, { hasuraPlan });
}

type IsFeatureEnabledProps<FEATURE extends Feature> = {
  /**
   * The name of the feature.
   */
  feature: FEATURE;

  /**
   * The children to render when the feature is enabled.
   */
  children: ReactElement;

  /**
   * A render function called when the feature is not enabled. It receives
   * - all the details of the check, including all the matches that did not pass
   * - the current state of the Hasura plan
   */
  ifDisabled?: (
    result: ReturnType<typeof useIsFeatureEnabled<FEATURE>>
  ) => ReactElement;
};

/**
 * Render the passed children when a feature is enabled and accept a `ifDisabled` render function
 * that receives an object containing all the details about the compatibilities and incompatibilities
 * with the current Hasura plan the Console is running.
 *
 * @example Simplest usage
 * function Prometheus() {
 *   return (
 *     <IsFeatureEnabled feature="prometheus">
 *       <div>Enjoy Prometheus!</div>
 *     </IsFeatureEnabled>
 *   );
 * }
 *
 * @example Advanced usage: render something different when the feature is disabled
 * function Prometheus() {
 *   return (
 *     <IsFeatureEnabled
 *       feature="prometheus"
 *       ifDisabled={(doNotMatch, current: { hasuraPlan }) => {
 *         if (doNotMatch.ee) {
 *           if(hasuraPlan.type === 'ce') {
 *             return <div>Try EE Lite and give all the paid feature a try for free!</div>
 *           }
 *
 *           return <div>Prometheus is enabled for EE Lite only</div>
 *         }
 *       }}
 *     >
 *       <div>Enjoy Prometheus!</div>
 *     </IsFeatureEnabled>
 *   );
 * }
 */
export function IsFeatureEnabled<FEATURE extends Feature>(
  props: IsFeatureEnabledProps<FEATURE>
) {
  const { feature: featureName, children, ifDisabled } = props;
  const result = useIsFeatureEnabled(featureName);

  if (result.status === 'enabled') {
    return children;
  }

  return ifDisabled?.(result) ?? null;
}
