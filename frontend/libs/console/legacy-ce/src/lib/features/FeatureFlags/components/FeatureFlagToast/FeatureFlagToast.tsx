import React from 'react';
import { Button } from '@hasura/shared/ui';
import { FaChevronRight } from 'react-icons/fa';
import { useFeatureFlagDismiss } from '../../hooks/useFeatureFlagDismiss';
import { useFeatureFlags } from '../../hooks/useFeatureFlags';
import { FeatureFlagDefinition, FeatureFlagId } from '../../types';
import { useNavigate } from 'react-router';
import { Flex } from '@radix-ui/themes';

interface FeatureFlagToastProps {
  flagId: FeatureFlagId;
  additionalFlags?: FeatureFlagDefinition[];
}

export const FeatureFlagToast = (props: FeatureFlagToastProps) => {
  const navigate = useNavigate();
  const { flagId, additionalFlags } = props;
  const [dismissed, setDismissed] = React.useState(false);
  const { isError, isLoading, data } = useFeatureFlags(additionalFlags);
  const setPermanentDismiss = useFeatureFlagDismiss();
  const featureFlag = data?.find((i) => i.id === flagId);
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
    <div className="fixed bottom-8 right-8 bg-white border overflow-hidden shadow-xl rounded-lg w-px-320 font-sans z-1">
      <Flex className="bg-primary px-4 py-3 align-middle">
        <h3 className="text-lg font-bold mb-0">
          Coming Soon: {featureFlag?.title}
        </h3>
      </Flex>
      <Flex
        align="center"
        justify="between"
        className="p-4 cursor-pointer"
        onClick={() => navigate('/settings/feature-flags')}
      >
        Try out the new feature before it gets to general availability.
        <FaChevronRight className="ml-2" aria-hidden="true" />
      </Flex>
      <Flex justify="between" className="p-4 border-t">
        <Button onClick={() => setDismissed(true)}>Hide for now</Button>
        <Button
          onClick={() => setPermanentDismiss.mutate(featureFlag?.id ?? '')}
        >
          Don&apos;t show me again
        </Button>
      </Flex>
    </div>
  );
};
