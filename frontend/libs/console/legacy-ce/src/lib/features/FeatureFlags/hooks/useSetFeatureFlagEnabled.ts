import { useMutation, useQueryClient } from 'react-query';
import { FeatureFlagId, useFeatureFlags } from '..';
import { saveFeatureFlagsStateToLocalStorage } from '../utils';
import { sendTelemetryEvent, SetFeatureFlagEvent } from '../../../telemetry';
import { availableFeatureFlags } from '../availableFeatureFlags';

export function useSetFeatureFlagEnabled() {
  const queryClient = useQueryClient();
  const { data, isError, isLoading } = useFeatureFlags();

  return useMutation(
    async ({
      flagId,
      newState,
    }: {
      flagId: FeatureFlagId;
      newState: boolean;
    }) => {
      if (isError || isLoading || !data) {
        throw new Error('Feature flags not loaded');
      }

      const setFeatureFlagEvent: SetFeatureFlagEvent = {
        type: 'SET_FEATURE_FLAG',
        data: {
          feature_flag:
            availableFeatureFlags.find(x => x.id === flagId)?.title ||
            'Unknown',
          value: newState,
        },
      };
      sendTelemetryEvent(setFeatureFlagEvent);

      const newFlags = data.map(item => {
        if (item.id !== flagId) {
          return item;
        }
        return {
          ...item,
          state: {
            ...item.state,
            enabled: newState,
            enableDate: new Date(),
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
