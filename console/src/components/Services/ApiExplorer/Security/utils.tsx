import React from 'react';
import { FaCheck, FaGlobe, FaTimes } from 'react-icons/fa';
import { HasuraMetadataV3 } from '../../../../metadata/types';
import { isEmpty } from '../../../Common/utils/jsUtils';
import { ApiLimitsFormSate } from './state';

const getApiLimits = (metadata: HasuraMetadataV3) => {
  const { node_limit, depth_limit, rate_limit, time_limit, batch_limit } =
    metadata.api_limits ?? {};
  return { depth_limit, node_limit, rate_limit, time_limit, batch_limit };
};

export enum RoleState {
  disabled = 'disabled',
  enabled = 'enabled',
  global = 'global',
}

export type RoleLimits = Omit<ApiLimitsFormSate, 'disabled'>;

const prepareApiLimits = (
  apiLimits: HasuraMetadataV3['api_limits']
): Omit<ApiLimitsFormSate, 'disabled'> => {
  const res = {} as Omit<ApiLimitsFormSate, 'disabled'>;
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
    apiLimits?.rate_limit?.global ??
    ({} as ApiLimitsFormSate['rate_limit']['global']);
  const rate_limit_per_role =
    apiLimits?.rate_limit?.per_role ??
    ({} as ApiLimitsFormSate['rate_limit']['per_role']);

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

export const getLimitsforRole =
  (metadata: HasuraMetadataV3) => (role: string) => {
    const limits = prepareApiLimits(getApiLimits(metadata));
    return Object.values(limits).map(value => {
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
        isEmpty(global) || global === -1
          ? RoleState.disabled
          : RoleState.enabled;
      return { global, state };
    });
  };

export const getLimitsforUnknownRole = (metadata: HasuraMetadataV3) => {
  const limits = prepareApiLimits(getApiLimits(metadata));
  return Object.values(limits).map(value => {
    const global = value.global;
    return {
      global,
      per_role: {},
      state:
        isEmpty(global) || global === -1
          ? RoleState.disabled
          : RoleState.global,
    };
  });
};

export const Legends = {
  Enabled: () => <FaCheck className="text-green-500" />,
  Disabled: () => <FaTimes className="text-red-500" />,
  Global: () => <FaGlobe className="text-gray-500" />,
};
