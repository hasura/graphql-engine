import * as React from 'react';
import { Dispatch } from '@/types';
import { useNeonIntegration } from '@/components/Services/Data/DataSources/CreateDataSource/Neon/useNeonIntegration';
import { transformNeonIntegrationStatusToNeonBannerProps } from '@/components/Services/Data/DataSources/CreateDataSource/Neon/utils';
import { reactQueryClient } from '@/lib/reactQuery';
import { FETCH_NEON_PROJECTS_BY_PROJECTID_QUERYKEY } from '@/components/Services/Data/DataSources/CreateDataSource/Neon/components/NeonDashboardLink';
import { NeonBanner } from '../../NeonConnectBanner/NeonBanner';
import _push from '../../../../../components/Services/Data/push';
import {
  useInstallTemplate,
  usePrefetchNeonOnboardingTemplateData,
  useEmitOnboardingEvents,
} from '../../../hooks';
import { NEON_TEMPLATE_BASE_PATH } from '../../../constants';
import { persistSkippedOnboarding } from '../../../utils';

export function NeonOnboarding(props: {
  dispatch: Dispatch;
  dismiss: VoidFunction;
  proceed: VoidFunction;
  setStepperIndex: (index: number) => void;
}) {
  const [installingTemplate, setInstallingTemplate] = React.useState(false);

  const { dispatch, dismiss, proceed, setStepperIndex } = props;

  const onSkipHandler = () => {
    persistSkippedOnboarding();
    dismiss();
  };

  const onSuccessHandler = () => {
    proceed();
  };

  const onErrorHanlder = () => {
    dispatch(_push('/data/manage/connect'));
    dismiss();
  };

  // Prefetch Neon related template data from github repo
  usePrefetchNeonOnboardingTemplateData(NEON_TEMPLATE_BASE_PATH);

  // Memoised function used to install the template
  const { install } = useInstallTemplate(
    'default',
    NEON_TEMPLATE_BASE_PATH,
    onSuccessHandler,
    onErrorHanlder
  );

  const neonIntegrationStatus = useNeonIntegration(
    'default',
    () => {
      // on success, refetch queries to show neon onboarding link in connect database page,
      // overriding the stale time
      reactQueryClient.refetchQueries(
        FETCH_NEON_PROJECTS_BY_PROJECTID_QUERYKEY
      );

      setInstallingTemplate(true);
      install();
    },
    () => {
      onErrorHanlder();
    },
    dispatch,
    'onboarding'
  );

  // emit onboarding events to the database
  useEmitOnboardingEvents(neonIntegrationStatus, installingTemplate);

  // allow skipping only when an action is not in-progress
  const allowSkipping =
    neonIntegrationStatus.status === 'idle' ||
    neonIntegrationStatus.status === 'authentication-error' ||
    neonIntegrationStatus.status === 'neon-database-creation-error';

  const neonBannerProps = transformNeonIntegrationStatusToNeonBannerProps(
    neonIntegrationStatus
  );

  // show template install status when template is installing
  neonBannerProps.buttonText = installingTemplate
    ? 'Installing Sample Schema'
    : neonBannerProps.buttonText;

  return (
    <div className="w-full">
      <div className="w-full mb-sm">
        <NeonBanner {...neonBannerProps} setStepperIndex={setStepperIndex} />
      </div>
      <div className="flex justify-start items-center w-full">
        <a
          className={`w-auto text-secondary text-sm hover:text-secondary-dark hover:no-underline ${
            allowSkipping ? 'cursor-pointer' : 'cursor-not-allowed'
          }`}
          data-trackid="onboarding-skip-button"
          title={!allowSkipping ? 'Please wait...' : undefined}
          onClick={() => {
            if (allowSkipping) {
              onSkipHandler();
            }
          }}
        >
          Skip getting started tutorial
        </a>
      </div>
    </div>
  );
}
