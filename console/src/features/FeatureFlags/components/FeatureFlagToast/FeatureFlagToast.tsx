import React from 'react';
import { useFeatureFlagDismiss } from '../../hooks/useFeatureFlagDismiss';
import { useFeatureFlags } from '../../hooks/useFeatureFlags';
import { FeatureFlagDefinition, FeatureFlagId } from '../../types';
import _push from '../../../../components/Services/Data/push';

interface FeatureFlagToastProps {
  flagId: FeatureFlagId;
  additionalFlags?: FeatureFlagDefinition[];
}

export const FeatureFlagToast = (props: FeatureFlagToastProps) => {
  const { flagId, additionalFlags } = props;
  const [dismissed, setDismissed] = React.useState(false);
  const { isError, isLoading, data } = useFeatureFlags(additionalFlags);
  const setPermanentDismiss = useFeatureFlagDismiss();
  const featureFlag = data?.find(i => i.id === flagId);
  if (
    isError ||
    isLoading ||
    dismissed ||
    !featureFlag ||
    featureFlag?.state.dismissed
  )
    return null;
  return (
    <div className="fixed bottom-8 right-8 bg-white border overflow-hidden shadow-xl rounded-lg w-px-320 font-sans">
      <div className="bg-primary px-4 py-3 flex align-middle">
        <h3 className="text-lg font-bold mb-0">
          Coming Soon: {featureFlag?.title}
        </h3>
      </div>
      <div
        className="flex p-4 cursor-pointer items-center justify-between"
        onClick={() => _push('/settings/feature-flags')}
      >
        Try out the new feature before it gets to general availability.
        <i className="fa fa-chevron-right ml-2" aria-hidden="true" />
      </div>
      <div className="flex p-4 justify-between border-t">
        <button onClick={() => setDismissed(true)}>Hide for now</button>
        <button
          className="text-gray-400"
          onClick={() => setPermanentDismiss.mutate(featureFlag?.id ?? '')}
        >
          Don&apos;t show me again
        </button>
      </div>
    </div>
  );
};
