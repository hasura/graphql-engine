/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ComparisonCapabilities } from './ComparisonCapabilities';
import type { ExplainCapabilities } from './ExplainCapabilities';
import type { GraphQLTypeDefinitions } from './GraphQLTypeDefinitions';
import type { MetricsCapabilities } from './MetricsCapabilities';
import type { MutationCapabilities } from './MutationCapabilities';
import type { QueryCapabilities } from './QueryCapabilities';
import type { RelationshipCapabilities } from './RelationshipCapabilities';
import type { ScalarTypesCapabilities } from './ScalarTypesCapabilities';
import type { SubscriptionCapabilities } from './SubscriptionCapabilities';

export type Capabilities = {
  comparisons?: ComparisonCapabilities;
  explain?: ExplainCapabilities;
  graphqlSchema?: GraphQLTypeDefinitions;
  metrics?: MetricsCapabilities;
  mutations?: MutationCapabilities;
  queries?: QueryCapabilities;
  relationships?: RelationshipCapabilities;
  scalarTypes?: ScalarTypesCapabilities;
  subscriptions?: SubscriptionCapabilities;
};

