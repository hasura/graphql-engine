import type { ApiLimitInput } from '@hasura/metadata/api';

export const inconsistentSourceObjects = [
  {
    definition: 'TestDB',
    reason: 'Inconsistent object: sql server exception',
    name: 'source TestDB',
    type: 'source',
  },
];

export const inconsistentTableObject = [
  {
    definition: {
      name: 'Table2',
      schema: 'TestSchema',
    },
    name: 'table TestSchema.Table2 in source default',
    reason: 'Inconsistent object: no such table/view exists in source',
    type: 'table',
  },
];

export const inconsistentRelationshipObject = [
  {
    definition: {
      name: 'relationshipName',
      source: 'default',
      Comment: null,
    },
    name: 'object_relation relationshipName in table TestSchema.Table1 in source default',
    reason: 'Inconsistent object: table "TestSchema.Table2" is not tracked',
    type: 'object_relation',
  },
];

export const inconsistentObject = [
  {
    definition: 'TestDB',
    reason: 'Inconsistent object: sql server exception',
    name: 'source TestDB',
    type: 'source',
  },
  {
    definition: {
      name: 'Table2',
      schema: 'TestSchema',
    },
    name: 'table TestSchema.Table2 in source default',
    reason: 'Inconsistent object: no such table/view exists in source',
    type: 'table',
  },
  {
    definition: {
      name: 'relationshipName',
      source: 'default',
      Comment: null,
    },
    name: 'object_relation relationshipName in table TestSchema.Table1 in source default',
    reason: 'Inconsistent object: table "TestSchema.Table2" is not tracked',
    type: 'object_relation',
  },
];

export const inconsistentRemoteSchema = [
  {
    name: `remote_schema_permission role permission in remote schema Test1`,
    reason: `Inconsistent object: remote schema  "Test1" does not exis`,
    type: 'remote_schema_permission',
  },
  {
    name: `remote_schema Test2`,
    reason: `Inconsistent object: Error in $: key "data" not found`,
    type: `remote_schema`,
  },
];

export const inconsistentRemoteRelation = [
  {
    definition: {
      name: 'relation_name',
      remote_schema: 'Test',
      source: 'default',
    },
    type: 'remote_relationship',
  },
];

export const multipleInconsistencyRemoteSchema = [
  {
    name: `remote_schema_permission role permission in remote schema Test`,
    reason: `Inconsistent object: remote schema  "Test" does not exis`,
    type: 'remote_schema_permission',
  },
  {
    name: `remote_schema Test`,
    reason: `Inconsistent object: Error in $: key "data" not found`,
    type: `remote_schema`,
  },
  {
    definition: {
      name: 'relation_name',
      remote_schema: 'Test',
      source: 'default',
    },
    type: 'remote_relationship',
  },
];

export const source = {
  name: 'default',
  driver: 'postgres',
} as const;

const api_limits: Record<string, ApiLimitInput> = {
  only_depth_limit: {
    existingAPILimits: undefined,
    newAPILimits: {
      disabled: false,
      depth_limit: { global: 10, state: 'enabled' },
      node_limit: { global: 10, state: 'disabled' },
      batch_limit: { global: 10, state: 'disabled' },
      rate_limit: {
        global: { unique_params: ['x-hasura-id'], max_reqs_per_min: 100 },
        state: 'disabled',
      },
    },
  },
  only_depth_limit_with_per_role: {
    existingAPILimits: undefined,
    newAPILimits: {
      disabled: false,
      depth_limit: {
        global: 10,
        state: 'enabled',
        per_role: {
          role1: 3,
        },
      },
      node_limit: { global: 10, state: 'disabled' },
      batch_limit: { global: 10, state: 'disabled' },
      rate_limit: {
        global: { unique_params: ['x-hasura-id'], max_reqs_per_min: 100 },
        state: 'disabled',
      },
    },
  },
  only_node_limit: {
    existingAPILimits: undefined,
    newAPILimits: {
      disabled: false,
      depth_limit: { global: 10, state: 'disabled' },
      node_limit: { global: 10, state: 'enabled' },
      batch_limit: { global: 10, state: 'disabled' },
      rate_limit: {
        global: { unique_params: ['x-hasura-id'], max_reqs_per_min: 100 },
        state: 'disabled',
      },
    },
  },
  only_node_limit_with_per_role: {
    existingAPILimits: undefined,
    newAPILimits: {
      disabled: false,
      depth_limit: { global: 10, state: 'disabled' },
      batch_limit: { global: 10, state: 'disabled' },
      node_limit: {
        global: 10,
        state: 'enabled',
        per_role: {
          role1: 3,
        },
      },
      rate_limit: {
        global: { unique_params: ['x-hasura-id'], max_reqs_per_min: 100 },
        state: 'disabled',
      },
    },
  },

  only_batch_limit: {
    existingAPILimits: undefined,
    newAPILimits: {
      disabled: false,
      depth_limit: { global: 10, state: 'disabled' },
      node_limit: { global: 10, state: 'disabled' },
      batch_limit: { global: 10, state: 'enabled' },
      rate_limit: {
        global: { unique_params: ['x-hasura-id'], max_reqs_per_min: 100 },
        state: 'disabled',
      },
    },
  },
  only_batch_limit_with_per_role: {
    existingAPILimits: undefined,
    newAPILimits: {
      disabled: false,
      depth_limit: { global: 10, state: 'disabled' },
      node_limit: { global: 10, state: 'disabled' },
      batch_limit: {
        global: 10,
        state: 'enabled',
        per_role: {
          role1: 3,
        },
      },
      rate_limit: {
        global: { unique_params: ['x-hasura-id'], max_reqs_per_min: 100 },
        state: 'disabled',
      },
    },
  },

  node_depth_and_batch_limits: {
    existingAPILimits: undefined,
    newAPILimits: {
      disabled: false,
      depth_limit: {
        global: 10,
        state: 'enabled',
        per_role: {
          role1: 2,
        },
      },
      node_limit: {
        global: 10,
        state: 'enabled',
        per_role: {
          role1: 3,
        },
      },
      batch_limit: {
        global: 10,
        state: 'enabled',
        per_role: {
          role1: 3,
        },
      },
      rate_limit: {
        global: { unique_params: ['x-hasura-id'], max_reqs_per_min: 100 },
        state: 'disabled',
      },
    },
  },
};

const introspection_options = {
  enable_for_role: {
    old_state: [],
    role: 'test_role',
    introspection_is_disabled: false,
  },
  state_is_present_and_enable_for_role: {
    old_state: ['existing_role'],
    role: 'test_role_1',
    introspection_is_disabled: false,
  },
  disable_for_role: {
    old_state: [],
    role: 'test_role',
    introspection_is_disabled: true,
  },
  state_is_present_and_disable_for_role: {
    old_state: ['existing_role'],
    role: 'test_role_1',
    introspection_is_disabled: true,
  },
};

export { api_limits, introspection_options };
