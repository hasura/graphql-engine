import { useFeatureFlags } from './useFeatureFlags';

export const useIsFeatureFlagEnabled = (
  id: string
): { enabled: undefined | boolean; isLoading: boolean } => {
  const { isLoading, data } = useFeatureFlags();
  if (isLoading) {
    return { enabled: undefined, isLoading: true };
  }
  const enabled = data?.find(ff => ff.id === id)?.state?.enabled;
  return { enabled, isLoading: false };
};
