/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ComparisonCapabilities } from './ComparisonCapabilities';
import type { ExplainCapabilities } from './ExplainCapabilities';
import type { GraphQLTypeDefinitions } from './GraphQLTypeDefinitions';
import type { MetricsCapabilities } from './MetricsCapabilities';
import type { MutationCapabilities } from './MutationCapabilities';
import type { QueryCapabilities } from './QueryCapabilities';
import type { RawCapabilities } from './RawCapabilities';
import type { RelationshipCapabilities } from './RelationshipCapabilities';
import type { ScalarTypesCapabilities } from './ScalarTypesCapabilities';
import type { SubscriptionCapabilities } from './SubscriptionCapabilities';

export type Capabilities = {
  comparisons?: ComparisonCapabilities;
  explain?: ExplainCapabilities;
  graphql_schema?: GraphQLTypeDefinitions;
  metrics?: MetricsCapabilities;
  mutations?: MutationCapabilities;
  queries?: QueryCapabilities;
  raw?: RawCapabilities;
  relationships?: RelationshipCapabilities;
  scalar_types?: ScalarTypesCapabilities;
  subscriptions?: SubscriptionCapabilities;
};

