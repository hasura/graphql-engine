// The various
// `as const satisfies Compatibility;`
// allow to use the object itself as the source of truth instead of the type, later allowing a
// better type DX with when consuming the `checkCompatibility` function.

import { type Compatibility } from './isFeatureEnabled';

/**
 * A fake feature that allows for testing the APIs.
 *
 * TODO: Remove this in favor of a real feature
 * @deprecated
 */
const unitTestActiveEeTrialFake = {
  ce: 'disabled',
  ee: {
    withLicense: 'enabled',
    withoutLicense: 'disabled',
  },
} as const satisfies Compatibility;

/**
 * A fake feature that allows for testing the APIs.
 *
 * TODO: Remove this in favor of a real feature
 * @deprecated
 */
const unitTestEeFake = {
  ce: 'disabled',
  ee: {
    withLicense: 'enabled',
    withoutLicense: 'enabled',
  },
} as const satisfies Compatibility;

/**
 * A fake feature that allows for testing the APIs.
 *
 * TODO: Remove this in favor of a real feature
 * @deprecated
 */
const unitTestCeFake = {
  ce: 'enabled',
  ee: {
    withLicense: 'disabled',
    withoutLicense: 'disabled',
  },
} as const satisfies Compatibility;

export const features = {
  unitTestCeFake,
  unitTestEeFake,
  unitTestActiveEeTrialFake,
} as const satisfies Record<string, Compatibility>;

export type Feature = keyof typeof features;
