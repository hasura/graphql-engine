import * as React from 'react';
import { Dispatch } from '@/types';
import { useNeonIntegration } from '@/components/Services/Data/DataSources/CreateDataSource/Neon/useNeonIntegration';
import { transformNeonIntegrationStatusToNeonBannerProps } from '@/components/Services/Data/DataSources/CreateDataSource/Neon/utils';
import { NeonBanner } from '../NeonConnectBanner/NeonBanner';
import _push from '../../../../components/Services/Data/push';

const useTemplateGallery = (
  onSuccess: VoidFunction,
  onError: VoidFunction,
  dispatch: Dispatch
) => {
  return {
    install: () => {
      dispatch(_push(`/data/default`));
    },
  };
};

export function NeonOnboarding(props: {
  dispatch: Dispatch;
  onSkip: VoidFunction;
  onCompletion: VoidFunction;
  onError: VoidFunction;
}) {
  const { dispatch, onSkip, onCompletion, onError } = props;

  // Sample function
  const { install } = useTemplateGallery(onCompletion, onError, dispatch);

  const neonIntegrationStatus = useNeonIntegration(
    'default',
    () => {
      install();
    },
    () => {
      onError();
      dispatch(_push(`/data/manage/connect`));
    },
    dispatch
  );

  const neonBannerProps = transformNeonIntegrationStatusToNeonBannerProps(
    neonIntegrationStatus
  );

  return (
    <div>
      <div className="w-full mb-sm">
        <NeonBanner {...neonBannerProps} />
      </div>
      <div className="flex justify-start items-center w-full">
        <a
          className="w-auto text-secondary cursor-pointer text-sm hover:text-secondary-dark"
          data-trackid="onboarding-skip-button"
          onClick={onSkip}
        >
          Skip setup, continue to console
        </a>
      </div>
    </div>
  );
}
