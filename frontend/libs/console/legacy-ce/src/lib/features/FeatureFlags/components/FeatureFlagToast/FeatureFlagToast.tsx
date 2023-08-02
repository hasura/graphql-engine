import React from 'react';
import { Button } from '../../../../new-components/Button';
import { useDispatch } from 'react-redux';
import { FaChevronRight } from 'react-icons/fa';
import { useFeatureFlagDismiss } from '../../hooks/useFeatureFlagDismiss';
import { useFeatureFlags } from '../../hooks/useFeatureFlags';
import { FeatureFlagDefinition, FeatureFlagId } from '../../types';
import _push from '../../../../components/Services/Data/push';

interface FeatureFlagToastProps {
  flagId: FeatureFlagId;
  additionalFlags?: FeatureFlagDefinition[];
}

export const FeatureFlagToast = (props: FeatureFlagToastProps) => {
  const dispatch = useDispatch();
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
    featureFlag.state.dismissed ||
    featureFlag.state.enabled
  )
    return null;
  return (
    <div className="fixed bottom-8 right-8 bg-white border overflow-hidden shadow-xl rounded-lg w-px-320 font-sans z-[1]">
      <div className="bg-primary px-4 py-3 flex align-middle">
        <h3 className="text-lg font-bold mb-0">
          Coming Soon: {featureFlag?.title}
        </h3>
      </div>
      <div
        className="flex p-4 cursor-pointer items-center justify-between"
        onClick={() => dispatch(_push('/settings/feature-flags'))}
      >
        Try out the new feature before it gets to general availability.
        <FaChevronRight className="ml-2" aria-hidden="true" />
      </div>
      <div className="flex p-4 justify-between border-t">
        <Button onClick={() => setDismissed(true)}>Hide for now</Button>
        <Button
          onClick={() => setPermanentDismiss.mutate(featureFlag?.id ?? '')}
        >
          Don&apos;t show me again
        </Button>
      </div>
    </div>
  );
};
