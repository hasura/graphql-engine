import { useQuery } from 'react-query';
import { getLSItem, LS_KEYS } from '../../../utils/localStorage';
import { isJsonString } from '../../../components/Common/utils/jsUtils';
import {
  FeatureFlagType,
  FeatureFlagDefinition,
  FeatureFlagState,
} from '../types';
import { availableFeatureFlags } from '../availableFeatureFlags';

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
  state: FeatureFlagState[]
): FeatureFlagType[] => {
  return flags.map(flag => {
    const flagState = state.find(f => f.id === flag.id);
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
  const flag = getFeatureFlags().find(ff => ff.id === id);

  if (!flag) return false;

  return flag.state.enabled;
};

export function getFeatureFlags(additionalFlags?: FeatureFlagDefinition[]) {
  const flags = getAvailableFeatureFlags();
  const state = getFeatureFlagStore();
  return mergeFlagWithState([...(additionalFlags ?? []), ...flags], state);
}

export function useFeatureFlags(additionalFlags?: FeatureFlagDefinition[]) {
  return useQuery(['featureFlags', 'all'], () =>
    getFeatureFlags(additionalFlags)
  );
}
