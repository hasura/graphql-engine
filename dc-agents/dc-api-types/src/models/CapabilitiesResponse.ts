/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Capabilities } from './Capabilities';
import type { ConfigSchemaResponse } from './ConfigSchemaResponse';

export type CapabilitiesResponse = {
  capabilities: Capabilities;
  config_schemas: ConfigSchemaResponse;
  display_name?: string;
  release_name?: string;
};

