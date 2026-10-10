import * as React from 'react';
import { useNeonIntegration } from '../../../hooks/useNeonIntegration';
import { transformNeonIntegrationStatusToNeonBannerProps } from '../../../hooks/utils';
import { Analytics } from '@hasura/shared/analytics';
import { NeonBanner } from '../../NeonConnectBanner/NeonBanner';
import {
  useInstallTemplate,
  usePrefetchNeonOnboardingTemplateData,
  useEmitOnboardingEvents,
} from '../../../hooks';
import {
  NEON_TEMPLATE_BASE_PATH,
  skippedNeonOnboardingVariables,
} from '../../../../constants';
import { emitOnboardingEvent } from '../../../../utils';
import { useNavigate } from 'react-router';
import { FETCH_NEON_PROJECTS_BY_PROJECTID_QUERYKEY } from '../../NeonDashboardLink';
import { Flex } from '@radix-ui/themes';
import { useQueryClient } from '@tanstack/react-query';

export function NeonOnboarding(props: {
  dismiss: VoidFunction;
  proceed: VoidFunction;
  setStepperIndex: (index: number) => void;
}) {
  const navigate = useNavigate();
  const queryClient = useQueryClient();
  const [installingTemplate, setInstallingTemplate] = React.useState(false);

  const { dismiss, proceed, setStepperIndex } = props;

  const onSkipHandler = () => {
    emitOnboardingEvent(skippedNeonOnboardingVariables);
    dismiss();
  };

  const onSuccessHandler = () => {
    proceed();
  };

  const onErrorHandler = () => {
    navigate('/data/manage/connect');
    dismiss();
  };

  const onInstallTemplateErrorHandler = () => {
    navigate('/data/default/schema/public');
    dismiss();
  };

  // Prefetch Neon related template data from github repo
  usePrefetchNeonOnboardingTemplateData(NEON_TEMPLATE_BASE_PATH);

  // Memoised function used to install the template
  const { install } = useInstallTemplate(
    'default',
    NEON_TEMPLATE_BASE_PATH,
    onSuccessHandler,
    onInstallTemplateErrorHandler,
  );

  const neonIntegrationStatus = useNeonIntegration(
    'default',
    () => {
      // on success, refetch queries to show neon dashboard link in connect database page,
      // overriding the stale time
      queryClient.refetchQueries({
        queryKey: FETCH_NEON_PROJECTS_BY_PROJECTID_QUERYKEY,
      });

      setInstallingTemplate(true);
      install();
    },
    () => {
      onErrorHandler();
    },
    'onboarding',
  );

  // emit onboarding events to the database
  useEmitOnboardingEvents(neonIntegrationStatus, installingTemplate);

  // allow skipping only when an action is not in-progress
  const isActionInProgress =
    neonIntegrationStatus.status !== 'idle' &&
    neonIntegrationStatus.status !== 'authentication-error' &&
    neonIntegrationStatus.status !== 'neon-database-creation-error';

  const neonBannerProps = transformNeonIntegrationStatusToNeonBannerProps(
    neonIntegrationStatus,
  );

  // show template install status when template is installing
  neonBannerProps.buttonText = installingTemplate
    ? 'Installing Sample Schema'
    : neonBannerProps.buttonText;

  return (
    <div className="w-full">
      <div className="w-full mb-2">
        <NeonBanner
          {...neonBannerProps}
          setStepperIndex={setStepperIndex}
          dismiss={dismiss}
        />
      </div>
      <Flex justify="start" align="center" className="w-full">
        <Analytics name="onboarding-skip-button">
          <a
            id="onboarding-skip-button"
            className={`w-auto text-secondary text-sm hover:text-secondary-dark hover:no-underline ${
              !isActionInProgress ? 'cursor-pointer' : 'cursor-not-allowed'
            }`}
            title={isActionInProgress ? 'Operation in progress...' : undefined}
            onClick={() => {
              if (!isActionInProgress) {
                onSkipHandler();
              }
            }}
          >
            Skip getting started tutorial
          </a>
        </Analytics>
      </Flex>
    </div>
  );
}
