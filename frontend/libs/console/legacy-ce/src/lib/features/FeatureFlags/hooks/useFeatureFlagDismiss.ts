import { useMutation, useQueryClient } from 'react-query';
import { FeatureFlagId, useFeatureFlags } from '..';
import { saveFeatureFlagsStateToLocalStorage } from '../utils';

export function useFeatureFlagDismiss() {
  const queryClient = useQueryClient();
  const { data, isError, isLoading } = useFeatureFlags();

  return useMutation(
    async (flagId: FeatureFlagId) => {
      if (isError || isLoading || !data) {
        throw new Error('Feature flags not loaded');
      }
      const newFlags = data.map(item => {
        if (item.id !== flagId) {
          return item;
        }
        return {
          ...item,
          state: {
            ...item.state,
            dismissed: true,
            dismissedDate: new Date(),
          },
        };
      });

      return saveFeatureFlagsStateToLocalStorage(newFlags);
    },
    {
      onSuccess: () => queryClient.invalidateQueries('featureFlags'),
    }
  );
}
