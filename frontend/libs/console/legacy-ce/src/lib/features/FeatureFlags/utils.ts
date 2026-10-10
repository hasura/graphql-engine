import { setLSItem } from '@hasura/shared/utils';
import { FeatureFlagType, FeatureFlagState } from './types';
import { LS_KEYS } from '@hasura/shared/types';

export const extractStateFromFeatureFlags = (
  flags: FeatureFlagType[],
): FeatureFlagState[] => {
  return flags.map((item) => ({ ...item.state, id: item.id }));
};

export const saveFeatureFlagsStateToLocalStorage = async (
  flags: FeatureFlagType[],
) => {
  const state = extractStateFromFeatureFlags(flags);
  setLSItem(LS_KEYS.featureFlag, JSON.stringify(state));
};
