import * as React from 'react';
import { Dispatch } from '../../../../../../types';
import { useNeonIntegration } from '../../../../../../components/Services/Data/DataSources/CreateDataSource/Neon/useNeonIntegration';
import { transformNeonIntegrationStatusToNeonBannerProps } from '../../../../../../components/Services/Data/DataSources/CreateDataSource/Neon/utils';
import { reactQueryClient } from '../../../../../../lib/reactQuery';
import { Analytics } from '../../../../../Analytics';
import { FETCH_NEON_PROJECTS_BY_PROJECTID_QUERYKEY } from '../../../../../../components/Services/Data/DataSources/CreateDataSource/Neon/components/NeonDashboardLink';
import _push from '../../../../../../components/Services/Data/push';
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

export function NeonOnboarding(props: {
  dispatch: Dispatch;
  dismiss: VoidFunction;
  proceed: VoidFunction;
  setStepperIndex: (index: number) => void;
}) {
  const [installingTemplate, setInstallingTemplate] = React.useState(false);

  const { dispatch, dismiss, proceed, setStepperIndex } = props;

  const onSkipHandler = () => {
    emitOnboardingEvent(skippedNeonOnboardingVariables);
    dismiss();
  };

  const onSuccessHandler = () => {
    proceed();
  };

  const onErrorHandler = () => {
    dispatch(_push('/data/manage/connect'));
    dismiss();
  };

  const onInstallTemplateErrorHandler = () => {
    dispatch(_push('/data/default/schema/public'));
    dismiss();
  };

  // Prefetch Neon related template data from github repo
  usePrefetchNeonOnboardingTemplateData(NEON_TEMPLATE_BASE_PATH);

  // Memoised function used to install the template
  const { install } = useInstallTemplate(
    'default',
    NEON_TEMPLATE_BASE_PATH,
    onSuccessHandler,
    onInstallTemplateErrorHandler
  );

  const neonIntegrationStatus = useNeonIntegration(
    'default',
    () => {
      // on success, refetch queries to show neon dashboard link in connect database page,
      // overriding the stale time
      reactQueryClient.refetchQueries(
        FETCH_NEON_PROJECTS_BY_PROJECTID_QUERYKEY
      );

      setInstallingTemplate(true);
      install();
    },
    () => {
      onErrorHandler();
    },
    dispatch,
    'onboarding'
  );

  // emit onboarding events to the database
  useEmitOnboardingEvents(neonIntegrationStatus, installingTemplate);

  // allow skipping only when an action is not in-progress
  const isActionInProgress =
    neonIntegrationStatus.status !== 'idle' &&
    neonIntegrationStatus.status !== 'authentication-error' &&
    neonIntegrationStatus.status !== 'neon-database-creation-error';

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
        <NeonBanner
          {...neonBannerProps}
          setStepperIndex={setStepperIndex}
          dispatch={dispatch}
          dismiss={dismiss}
        />
      </div>
      <div className="flex justify-start items-center w-full">
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
      </div>
    </div>
  );
}
