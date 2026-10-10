import { useState } from 'react';
import { RoleLimits, RoleState } from '../utils';
import type { APILimit, ApiLimits, RateLimit } from '@hasura/shared/types';
import type { ApiLimitInput } from '@hasura/metadata/api';

type LimitPayload<T = number> = {
  role: string;
  limit: T;
};

const createApiLimitState = (): APILimit<number> => ({
  global: -1,
  state: RoleState.disabled,
  per_role: {} as Record<string, number>,
});

const createRateLimitState = (): RateLimit => ({
  global: {},
  state: RoleState.disabled,
  per_role: {},
});

const useAPILimitForm = () => {
  const [depthLimit, setDepthLimit] = useState(createApiLimitState());
  const [batchLimit, setBatchLimit] = useState(createApiLimitState());
  const [nodeLimit, setNodeLimit] = useState(createApiLimitState());
  const [timeLimit, setTimeLimit] = useState(createApiLimitState());
  const [rateLimit, setRateLimit] = useState(createRateLimitState());

  const resetForm = (state: ApiLimits) => {
    setDepthLimit(state.depth_limit ?? createApiLimitState());
    setNodeLimit(state.node_limit ?? createApiLimitState());
    setTimeLimit(state.time_limit ?? createApiLimitState());
    setBatchLimit(state.batch_limit ?? createApiLimitState());
    setRateLimit(state.rate_limit ?? createRateLimitState());
  };

  const getState = (disabled: boolean): ApiLimitInput['newAPILimits'] => ({
    disabled,
    depth_limit: depthLimit,
    batch_limit: batchLimit,
    node_limit: nodeLimit,
    rate_limit: rateLimit,
    time_limit: timeLimit,
  });

  const updateGlobalDepthLimit = (value: number) => {
    setDepthLimit({
      ...depthLimit,
      global: value,
    });
  };

  const updateDepthLimitRole = (payload: LimitPayload) => {
    setDepthLimit({
      ...depthLimit,
      per_role: {
        ...depthLimit.per_role,
        [payload.role]: payload.limit,
      },
    });
  };

  const updateDepthLimitState = (value: RoleState) => {
    setDepthLimit({
      ...depthLimit,
      state: value,
    });
  };

  const updateGlobalBatchLimit = (value: number) => {
    setBatchLimit({
      ...batchLimit,
      global: value,
    });
  };

  const updateBatchLimitRole = (payload: LimitPayload) => {
    setBatchLimit({
      ...batchLimit,
      per_role: {
        ...batchLimit.per_role,
        [payload.role]: payload.limit,
      },
    });
  };

  const updateBatchLimitState = (value: RoleState) => {
    setBatchLimit({
      ...batchLimit,
      state: value,
    });
  };

  const updateGlobalNodeLimit = (value: number) => {
    setNodeLimit({
      ...nodeLimit,
      global: value,
    });
  };

  const updateNodeLimitRole = (payload: LimitPayload) => {
    setNodeLimit({
      ...nodeLimit,
      per_role: {
        ...nodeLimit.per_role,
        [payload.role]: payload.limit,
      },
    });
  };

  const updateTimeLimitRole = (payload: LimitPayload) => {
    setTimeLimit({
      ...timeLimit,
      per_role: {
        ...timeLimit.per_role,
        [payload.role]: payload.limit,
      },
    });
  };

  const updateNodeLimitState = (value: RoleState) => {
    setNodeLimit({
      ...nodeLimit,
      state: value,
    });
  };

  const updateTimeLimitState = (value: RoleState) => {
    setTimeLimit({
      ...timeLimit,
      state: value,
    });
  };

  const updateGlobalTimeLimit = (value: number) => {
    setTimeLimit({
      ...timeLimit,
      global: value,
    });
  };

  const updateUniqueParams = (payload: LimitPayload<'IP' | string[]>) => {
    setRateLimit({
      ...rateLimit,
      per_role: {
        ...rateLimit.per_role,
        [payload.role]: {
          ...(rateLimit.per_role?.[payload.role] ?? { max_reqs_per_min: 1 }),
          unique_params: payload.limit,
        },
      },
    });
  };

  const updateMaxReqPerMin = (payload: LimitPayload) => {
    setRateLimit({
      ...rateLimit,
      per_role: {
        ...rateLimit.per_role,
        [payload.role]: {
          ...(rateLimit.per_role?.[payload.role] ?? { unique_params: null }),
          max_reqs_per_min: payload.limit,
        },
      },
    });
  };

  const updateGlobalUniqueParams = (value: 'IP' | string[]) => {
    setRateLimit({
      ...rateLimit,
      global: {
        ...rateLimit.global,
        unique_params: value,
      },
    });
  };

  const updateGlobalMaxReqPerMin = (value: number) => {
    setRateLimit({
      ...rateLimit,
      global: {
        ...rateLimit.global,
        max_reqs_per_min: value,
      },
    });
  };

  const updateRateLimitState = (state: RoleState) => {
    setRateLimit({
      ...rateLimit,
      state,
    });
  };

  const changeRoleState = (limit: keyof RoleLimits, state: RoleState) => {
    switch (limit) {
      case 'depth_limit':
        return updateDepthLimitState(state);
      case 'node_limit':
        return updateNodeLimitState(state);
      case 'batch_limit':
        return updateBatchLimitState(state);
      case 'time_limit':
        return updateTimeLimitState(state);
      default:
        return updateRateLimitState(state);
    }
  };

  const changeRoleLimit = (limit: keyof RoleLimits, payload: LimitPayload) => {
    switch (limit) {
      case 'depth_limit':
        return updateDepthLimitRole(payload);
      case 'node_limit':
        return updateNodeLimitRole(payload);
      case 'batch_limit':
        return updateBatchLimitRole(payload);
      case 'time_limit':
        return updateTimeLimitRole(payload);
      default:
        return updateMaxReqPerMin(payload);
    }
  };

  const changeGlobalLimit = (limit: keyof RoleLimits, value: number) => {
    switch (limit) {
      case 'depth_limit':
        return updateGlobalDepthLimit(value);
      case 'node_limit':
        return updateGlobalNodeLimit(value);
      case 'batch_limit':
        return updateGlobalBatchLimit(value);
      case 'time_limit':
        return updateGlobalTimeLimit(value);
      default:
        return updateGlobalMaxReqPerMin(value);
    }
  };

  const getApiLimitByKey = (limit: keyof RoleLimits) => {
    switch (limit) {
      case 'depth_limit':
        return depthLimit;
      case 'node_limit':
        return nodeLimit;
      case 'batch_limit':
        return batchLimit;
      case 'time_limit':
        return timeLimit;
      default:
        return rateLimit;
    }
  };

  return {
    depthLimit,
    batchLimit,
    nodeLimit,
    rateLimit,
    timeLimit,
    getState,
    resetForm,
    updateUniqueParams,
    updateGlobalUniqueParams,
    changeRoleState,
    changeRoleLimit,
    changeGlobalLimit,
    getApiLimitByKey,
  };
};

export default useAPILimitForm;
