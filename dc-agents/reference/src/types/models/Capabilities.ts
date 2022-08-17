/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { FilteringCapabilities } from './FilteringCapabilities';
import type { MetricsCapabilities } from './MetricsCapabilities';
import type { MutationCapabilities } from './MutationCapabilities';
import type { QueryCapabilities } from './QueryCapabilities';
import type { RelationshipCapabilities } from './RelationshipCapabilities';
import type { SubscriptionCapabilities } from './SubscriptionCapabilities';

export type Capabilities = {
  filtering?: FilteringCapabilities;
  metrics?: MetricsCapabilities;
  mutations?: MutationCapabilities;
  queries?: QueryCapabilities;
  relationships?: RelationshipCapabilities;
  subscriptions?: SubscriptionCapabilities;
};

