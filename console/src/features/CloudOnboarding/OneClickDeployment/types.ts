import {
  FetchOneClickDeploymentStateLogSubscriptionSubscription,
  One_Click_Deployment_States_Enum as OneClickDeploymentState,
} from '@/features/ControlPlane';

export type OneClickDeployment = NonNullable<
  FetchOneClickDeploymentStateLogSubscriptionSubscription['one_click_deployment_by_pk']
>;
export type OneClickDeploymentStateTransition =
  OneClickDeployment['one_click_deployment_state_logs'][0];
export { OneClickDeploymentState };

export type UserFacingStep = Exclude<
  OneClickDeploymentState,
  OneClickDeploymentState.Error
>;

export type RequiredEnvVar = {
  Kind: string;
  Name: string;
  Default?: string;
  SubKind?: string;
  Mandatory?: boolean;
  Description?: string;
};

export type CliLog =
  | {
      kind: 'loading';
      step: OneClickDeploymentState;
    }
  | {
      kind: 'success';
      step: OneClickDeploymentState;
    }
  | {
      kind: 'awaiting-env';
      step: OneClickDeploymentState;
      payload: RequiredEnvVar[];
    }
  | {
      kind: 'error';
      step: OneClickDeploymentState;
      payload: Record<string, any>;
    };

export type ProgressStateStatus =
  | {
      kind: 'in-progress';
    }
  | {
      kind: 'error';
      error: Record<string, any>;
    }
  | {
      kind: 'awaiting';
      payload: RequiredEnvVar[];
    }
  | {
      kind: 'success';
    }
  | {
      kind: 'idle';
    };

export type ProgressState = Record<UserFacingStep, ProgressStateStatus>;

export type EnvVarsFormState =
  | 'default'
  | 'loading'
  | 'success'
  | 'error'
  | 'hidden';

export type GitRepoDetails = {
  url: string;
  branch?: string;
  hasuraDirectory?: string;
};
