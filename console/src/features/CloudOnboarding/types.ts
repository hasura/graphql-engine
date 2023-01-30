import { OnboardingResponseData } from './OnboardingWizard';
import { GitRepoDetails, FallbackApp } from './OneClickDeployment';

export type OneClickDeploymentSampleApp =
  OnboardingResponseData['data']['one_click_deployment_sample_apps'];

export type OnboardingKind =
  | {
      kind: 'wizard';
    }
  | {
      kind: 'one-click-deployment';
      deployment: {
        deploymentId: number;
        gitRepoDetails: GitRepoDetails;
      };
      fallbackApps: OnboardingResponseData['data']['one_click_deployment_sample_apps'];
    }
  | {
      kind: 'none';
    };

export { FallbackApp };
