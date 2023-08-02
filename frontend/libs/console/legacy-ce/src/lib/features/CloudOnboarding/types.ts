import { One_Click_Deployment_States_Enum } from '../ControlPlane';
import { GitRepoDetails, FallbackApp } from './OneClickDeployment';

export type UserOnboarding = {
  activity: Record<string, any>;
  target: string;
  is_onboarded: boolean;
};

export type User = {
  id: string;
  created_at: string;
};

export type OneClickDeploymentByProject = {
  id: number;
  state: One_Click_Deployment_States_Enum;
  git_repository_url: string;
  git_repository_branch?: string;
  hasura_directory?: string;
};

export type OneClickDeploymentSampleApp = {
  git_repository_url: string;
  git_repository_branch: string;
  hasura_directory: string;
  name: string;
  description?: string;
  react_icons_fa_component_name?: string;
  rank: number;
};

export type OnboardingResponseData = {
  data: {
    user_onboarding: UserOnboarding[];
    users: User[];
    one_click_deployment: OneClickDeploymentByProject[];
    one_click_deployment_sample_apps: OneClickDeploymentSampleApp[];
  };
};

export type OnboardingKind =
  | {
      kind: 'neon-onboarding';
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
      kind: 'use-case-onboarding';
    }
  | {
      kind: 'none';
    };

export { FallbackApp };
