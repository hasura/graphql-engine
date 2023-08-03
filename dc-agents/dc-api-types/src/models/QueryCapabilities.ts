/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ForeachCapabilities } from './ForeachCapabilities';
import type { RedactionCapabilities } from './RedactionCapabilities';

export type QueryCapabilities = {
  foreach?: ForeachCapabilities;
  redaction?: RedactionCapabilities;
};

