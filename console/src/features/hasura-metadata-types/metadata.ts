import { InheritedRole } from './inheritedRoles';
import { AllowList } from './allowList';
import { QueryCollection } from './queryCollections';
import { Source } from './source';
import { BackendConfigs } from './backendConfigs';
import { RemoteSchema } from './remoteSchemas';
import { Action, CustomTypes } from './actions';
import { CronTrigger } from './cronTriggers';
import { Network } from './network';
import { RestEndpoint } from './restEndpoints';
import { ApiLimits } from './apiLimits';
import { GraphQLSchemaIntrospection } from './graphqlSchemaIntrospection';

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
  };
};
