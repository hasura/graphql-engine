import * as React from 'react';
import { Dispatch } from '@/types';
import { useNeonIntegration } from '@/components/Services/Data/DataSources/CreateDataSource/Neon/useNeonIntegration';
import { transformNeonIntegrationStatusToNeonBannerProps } from '@/components/Services/Data/DataSources/CreateDataSource/Neon/utils';
import { NeonBanner } from '../NeonConnectBanner/NeonBanner';
import _push from '../../../../components/Services/Data/push';
import {
  useInstallTemplate,
  usePrefetchNeonOnboardingTemplateData,
} from '../../hooks';

export function NeonOnboarding(props: {
  dispatch: Dispatch;
  onSkip: VoidFunction;
  onCompletion: VoidFunction;
  onError: (errorMsg?: string) => void;
}) {
  const { dispatch, onSkip, onCompletion, onError } = props;

  // Prefetch Neon related template data from github repo
  usePrefetchNeonOnboardingTemplateData();

  // Memoised function used to install the template
  const { install } = useInstallTemplate('default', onCompletion, onError);

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
