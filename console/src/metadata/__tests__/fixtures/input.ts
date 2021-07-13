import { HasuraMetadataV3 } from '../../types';
import { APILimitInputType } from '../../utils';

type apiLimitsType = {
  old_state: HasuraMetadataV3['api_limits'];
  new_state: {
    disabled: boolean;
    depth_limit: APILimitInputType<number>;
    node_limit: APILimitInputType<number>;
    rate_limit: APILimitInputType<{
      unique_params: 'IP' | string[];
      max_reqs_per_min: number;
    }>;
  };
};

const api_limits: Record<string, apiLimitsType> = {
  only_depth_limit: {
    old_state: undefined,
    new_state: {
      disabled: false,
      depth_limit: { global: 10, state: 'enabled' },
      node_limit: { global: 10, state: 'disabled' },
      rate_limit: {
        global: { unique_params: ['x-hasura-id'], max_reqs_per_min: 100 },
        state: 'disabled',
      },
    },
  },
  only_depth_limit_with_per_role: {
    old_state: undefined,
    new_state: {
      disabled: false,
      depth_limit: {
        global: 10,
        state: 'enabled',
        per_role: {
          role1: 3,
        },
      },
      node_limit: { global: 10, state: 'disabled' },
      rate_limit: {
        global: { unique_params: ['x-hasura-id'], max_reqs_per_min: 100 },
        state: 'disabled',
      },
    },
  },
  only_node_limit: {
    old_state: undefined,
    new_state: {
      disabled: false,
      depth_limit: { global: 10, state: 'disabled' },
      node_limit: { global: 10, state: 'enabled' },
      rate_limit: {
        global: { unique_params: ['x-hasura-id'], max_reqs_per_min: 100 },
        state: 'disabled',
      },
    },
  },
  only_node_limit_with_per_role: {
    old_state: undefined,
    new_state: {
      disabled: false,
      depth_limit: { global: 10, state: 'disabled' },
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
  node_and_depth_limits: {
    old_state: undefined,
    new_state: {
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
      rate_limit: {
        global: { unique_params: ['x-hasura-id'], max_reqs_per_min: 100 },
        state: 'disabled',
      },
    },
  },
};

export { api_limits };
