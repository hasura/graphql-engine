/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ComparisonCapabilities } from './ComparisonCapabilities';
import type { DataSchemaCapabilities } from './DataSchemaCapabilities';
import type { DatasetCapabilities } from './DatasetCapabilities';
import type { ExplainCapabilities } from './ExplainCapabilities';
import type { InterpolatedQueryCapabilities } from './InterpolatedQueryCapabilities';
import type { Licensing } from './Licensing';
import type { MetricsCapabilities } from './MetricsCapabilities';
import type { MutationCapabilities } from './MutationCapabilities';
import type { PostSchemaCapabilities } from './PostSchemaCapabilities';
import type { QueryCapabilities } from './QueryCapabilities';
import type { RawCapabilities } from './RawCapabilities';
import type { RelationshipCapabilities } from './RelationshipCapabilities';
import type { ScalarTypesCapabilities } from './ScalarTypesCapabilities';
import type { SubscriptionCapabilities } from './SubscriptionCapabilities';
import type { UserDefinedFunctionCapabilities } from './UserDefinedFunctionCapabilities';

export type Capabilities = {
  comparisons?: ComparisonCapabilities;
  data_schema?: DataSchemaCapabilities;
  datasets?: DatasetCapabilities;
  explain?: ExplainCapabilities;
  interpolated_queries?: InterpolatedQueryCapabilities;
  licensing?: Licensing;
  metrics?: MetricsCapabilities;
  mutations?: MutationCapabilities;
  post_schema?: PostSchemaCapabilities;
  queries?: QueryCapabilities;
  raw?: RawCapabilities;
  relationships?: RelationshipCapabilities;
  scalar_types?: ScalarTypesCapabilities;
  subscriptions?: SubscriptionCapabilities;
  user_defined_functions?: UserDefinedFunctionCapabilities;
};

