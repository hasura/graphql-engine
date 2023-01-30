/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { AtomicitySupportLevel } from './AtomicitySupportLevel';
import type { DeleteCapabilities } from './DeleteCapabilities';
import type { InsertCapabilities } from './InsertCapabilities';
import type { ReturningCapabilities } from './ReturningCapabilities';
import type { UpdateCapabilities } from './UpdateCapabilities';

export type MutationCapabilities = {
  atomicity_support_level?: AtomicitySupportLevel;
  delete?: DeleteCapabilities;
  insert?: InsertCapabilities;
  returning?: ReturningCapabilities;
  update?: UpdateCapabilities;
};

