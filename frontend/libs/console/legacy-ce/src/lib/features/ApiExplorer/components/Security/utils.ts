import { isEmpty } from '@hasura/shared/utils';
import { ApiLimits, Metadata, RateLimit } from '@hasura/shared/types';

const getApiLimits = (metadata: Metadata['metadata']) => {
  const { node_limit, depth_limit, rate_limit, time_limit, batch_limit } =
    metadata.api_limits ?? {};
  return { depth_limit, node_limit, rate_limit, time_limit, batch_limit };
};

export enum RoleState {
  disabled = 'disabled',
  enabled = 'enabled',
  global = 'global',
}

export type RoleLimits = Omit<ApiLimits, 'disabled'>;

const prepareApiLimits = (apiLimits: ApiLimits): RoleLimits => {
  const res = {} as RoleLimits;
  res.depth_limit = {
    global: apiLimits?.depth_limit?.global ?? -1,
    state: RoleState.disabled,
    per_role: apiLimits?.depth_limit?.per_role ?? {},
  };
  res.node_limit = {
    global: apiLimits?.node_limit?.global ?? -1,
    state: RoleState.disabled,
    per_role: apiLimits?.node_limit?.per_role ?? {},
  };

  const rate_limit_global =
    apiLimits?.rate_limit?.global ?? ({} as RateLimit['global']);
  const rate_limit_per_role =
    apiLimits?.rate_limit?.per_role ?? ({} as RateLimit['per_role']);

  res.rate_limit = {
    global: rate_limit_global,
    per_role: rate_limit_per_role,
    state: RoleState.disabled,
  };

  res.time_limit = {
    global: apiLimits?.time_limit?.global ?? -1,
    state: RoleState.disabled,
    per_role: apiLimits?.time_limit?.per_role ?? {},
  };

  res.batch_limit = {
    global: apiLimits?.batch_limit?.global ?? -1,
    state: RoleState.disabled,
    per_role: apiLimits?.batch_limit?.per_role ?? {},
  };

  return res;
};

export const getLimitsforRole = (meta: Metadata) => (role: string) => {
  const limits = prepareApiLimits(getApiLimits(meta.metadata));
  return Object.values(limits).map((value) => {
    if (role !== 'global') {
      const global = value.global;
      const per_role = value?.per_role?.[role];
      const state =
        isEmpty(global) || global === -1
          ? RoleState.disabled
          : isEmpty(per_role)
            ? RoleState.global
            : RoleState.enabled;
      return { global, per_role: { [role]: per_role }, state };
    }
    const global = value.global;
    const state =
      isEmpty(global) || global === -1 ? RoleState.disabled : RoleState.enabled;
    return { global, state };
  });
};

// System session variables can't be used as rate limit unique parameters.
export const RESERVED_SESSION_VARIABLES = [
  'x-hasura-role',
  'x-hasura-admin-secret',
  'x-hasura-access-key',
  'x-hasura-use-backend-only-permissions',
];

export const isReservedSessionVariable = (param: string) =>
  RESERVED_SESSION_VARIABLES.includes(param.trim().toLowerCase());

export const getReservedSessionVariables = (
  uniqueParams?: 'IP' | string[] | null,
): string[] =>
  Array.isArray(uniqueParams)
    ? uniqueParams.filter(isReservedSessionVariable)
    : [];

// Returns the validation error for rate limit unique parameters, if any.
export const validateUniqueParams = (
  uniqueParams?: 'IP' | string[] | null,
): string | null => {
  if (!Array.isArray(uniqueParams)) return null;
  if (uniqueParams.some((param) => !param.trim())) {
    return 'Session variables cannot be empty.';
  }
  const reserved = getReservedSessionVariables(uniqueParams);
  if (reserved.length > 0) {
    return `System session variables can't be used as unique parameters: ${reserved.join(', ')}`;
  }
  return null;
};
