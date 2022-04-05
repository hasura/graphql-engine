import { useQuery } from 'react-query';
import { getLSItem, LS_KEYS } from '@/utils/localStorage';
import { isJsonString } from '@/components/Common/utils/jsUtils';
import {
  FeatureFlagType,
  FeatureFlagDefinition,
  FeatureFlagState,
} from '../types';
import { availableFeatureFlags } from '../availableFeatureFlags';

const getFeatureFlagStore = async (): Promise<FeatureFlagState[]> => {
  const flagsFromLocalStorageAsString = getLSItem(LS_KEYS.featureFlag) ?? '';
  const content = isJsonString(flagsFromLocalStorageAsString)
    ? JSON.parse(flagsFromLocalStorageAsString)
    : [];

  if (!Array.isArray(content)) {
    return [];
  }
  return content;
};

// Async here to act as a placeholder when we will have an API
const getAvailableFeatureFlags = async (): Promise<FeatureFlagDefinition[]> => {
  return availableFeatureFlags;
};

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

export function useFeatureFlags(additionalFlags?: FeatureFlagDefinition[]) {
  return useQuery(['featureFlags', 'all'], () =>
    Promise.all([
      getAvailableFeatureFlags(),
      getFeatureFlagStore(),
    ]).then(([flags, state]) =>
      mergeFlagWithState([...(additionalFlags ?? []), ...flags], state)
    )
  );
}
