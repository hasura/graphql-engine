import { useQuery } from '@tanstack/react-query';
import { getLSItem, isJsonString } from '@hasura/shared/utils';
import {
  FeatureFlagType,
  FeatureFlagDefinition,
  FeatureFlagState,
} from '../types';
import { availableFeatureFlags } from '../availableFeatureFlags';
import { LS_KEYS } from '@hasura/shared/types';

const getFeatureFlagStore = (): FeatureFlagState[] => {
  const flagsFromLocalStorageAsString = getLSItem(LS_KEYS.featureFlag) ?? '';
  const content = isJsonString(flagsFromLocalStorageAsString)
    ? JSON.parse(flagsFromLocalStorageAsString)
    : [];

  if (!Array.isArray(content)) {
    return [];
  }
  return content;
};

const getAvailableFeatureFlags = (): FeatureFlagDefinition[] =>
  availableFeatureFlags;

export const mergeFlagWithState = (
  flags: FeatureFlagDefinition[],
  state: FeatureFlagState[],
): FeatureFlagType[] => {
  return flags.map((flag) => {
    const flagState = state.find((f) => f.id === flag.id);
    return {
      ...flag,
      state: flagState ?? {
        enabled: flag.defaultValue,
        dismissed: false,
      },
    };
  });
};

export const isFeatureFlagEnabled = (id: string) => {
  const flag = getFeatureFlags().find((ff) => ff.id === id);

  if (!flag) return false;

  return flag.state.enabled;
};

export function getFeatureFlags(additionalFlags?: FeatureFlagDefinition[]) {
  const flags = getAvailableFeatureFlags();
  const state = getFeatureFlagStore();
  return mergeFlagWithState([...(additionalFlags ?? []), ...flags], state);
}

export function useFeatureFlags(additionalFlags?: FeatureFlagDefinition[]) {
  return useQuery({
    queryKey: ['featureFlags', 'all'],
    queryFn: () => getFeatureFlags(additionalFlags),
  });
}
