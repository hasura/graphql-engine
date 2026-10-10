import React from 'react';
import globals from '../../../Globals';
import { FaGithub } from 'react-icons/fa';
import { GraphiqlPopup, WorkflowProgress } from './components';
import { stepperNavSteps } from './constants';
import { oneClickDeploymentOnboardingShown } from '../constants';
import { DialogContainer } from '../components';
import { useTriggerDeployment } from './hooks';
import { GitRepoDetails, FallbackApp } from './types';
import {
  getGitRepoFromUrl,
  getGitRepoFullLinkFromDetails,
  getSampleQueriesUrl,
} from './util';
import { emitOnboardingEvent } from '../utils';
import { Link } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

/**
 * Parent container for the one click deployment wizard. Takes care of assembling and rendering all steps.
 */
export function Root(props: {
  deployment: {
    deploymentId: string;
    gitRepoDetails: GitRepoDetails;
  };
  dismissOnboarding: VoidFunction;
  fallbackApps: FallbackApp[];
}) {
  const { deployment, dismissOnboarding, fallbackApps } = props;
  const { deploymentId, gitRepoDetails } = deployment;

  const [state, setState] = React.useState<
    'deployment-progress' | 'graphiql-popup'
  >('deployment-progress');
  const [graphiQlPopupStatus, setGraphiQlPopupStatus] = React.useState<
    'success' | 'error'
  >('success');

  const projectId = globals.hasuraCloudProjectId || '';
  const { triggerDeployment } = useTriggerDeployment(projectId);

  const transitionToQueryPopupSuccessState = () => {
    setGraphiQlPopupStatus('success');
    setState('graphiql-popup');
    emitOnboardingEvent(oneClickDeploymentOnboardingShown);
  };
  const transitionToQueryPopupWithErrorState = () => {
    setGraphiQlPopupStatus('error');
    setState('graphiql-popup');
  };

  const gitRepoName = React.useMemo(
    () => getGitRepoFromUrl(gitRepoDetails.url),
    [gitRepoDetails.url],
  );

  const gitRepoFullLink = React.useMemo(
    () => getGitRepoFullLinkFromDetails(gitRepoDetails),
    [gitRepoDetails],
  );

  const [stepperIndex, setStepperIndex] = React.useState<number>(1);
  switch (state) {
    case 'deployment-progress': {
      return (
        <DialogContainer
          showStepper
          stepperNavSteps={stepperNavSteps}
          activeIndex={stepperIndex}
          header="Setting up your project"
          showSubHeaderAboveHeader
          subHeader={
            <Link
              color="gray"
              href={gitRepoFullLink}
              target="_blank"
              rel="noopener noreferrer"
              underline="none"
            >
              <Flex align="center" gap="1">
                <FaGithub />
                <span>{gitRepoName}</span>
              </Flex>
            </Link>
          }
        >
          <WorkflowProgress
            setStepperIndex={setStepperIndex}
            deploymentId={deploymentId}
            projectId={projectId}
            gitRepoName={gitRepoName}
            sampleQueriesFileUrl={getSampleQueriesUrl(gitRepoDetails)}
            onCompleteSuccess={transitionToQueryPopupSuccessState}
            onCompleteError={transitionToQueryPopupWithErrorState}
            fallbackApps={fallbackApps}
          />
        </DialogContainer>
      );
    }
    case 'graphiql-popup': {
      return (
        <GraphiqlPopup
          status={graphiQlPopupStatus}
          gitRepoName={gitRepoName}
          gitRepoFullLink={gitRepoFullLink}
          retryCb={triggerDeployment}
          dismissCb={dismissOnboarding}
        />
      );
    }
    default: {
      return null;
    }
  }
}
