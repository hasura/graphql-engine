import type { InheritedRole } from './inheritedRoles';
import type { AllowList } from './allowList';
import type { QueryCollection } from './queryCollections';
import type { Source } from './source';
import type { BackendConfigs } from './backendConfigs';
import type { RemoteSchema } from './remoteSchemas';
import type { Action, CustomTypes } from './actions';
import type { CronTrigger } from './cronTriggers';
import type { Network } from './network';
import type { RestEndpoint } from './restEndpoints';
import type { ApiLimits } from './apiLimits';
import type { GraphQLSchemaIntrospection } from './graphqlSchemaIntrospection';
import type { OpenTelemetry } from './openTelemetry';

export type Metadata = {
  resource_version: number;
  metadata: {
    version: 3;
    sources: Source[];
    backend_configs?: BackendConfigs;
    remote_schemas?: RemoteSchema[];
    actions?: Action[];
    query_collections?: QueryCollection[];
    allowlist?: AllowList[];
    inherited_roles?: InheritedRole[];
    custom_types?: CustomTypes;
    cron_triggers?: CronTrigger[];
    network?: Network;
    rest_endpoints?: RestEndpoint[];
    api_limits?: ApiLimits;
    graphql_schema_introspection?: GraphQLSchemaIntrospection;

    /**
     * The EE Lite OpenTelemetry settings.
     *
     * ATTENTION: Both Lux and the EE Lite server allow configuring OpenTelemetry. Anyway, this only
     * represents the EE Lite one since Lux stores the OpenTelemetry settings by itself.
     */
    opentelemetry?: OpenTelemetry;
  };
};
