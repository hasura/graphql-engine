import { Driver } from '../../../dataSources';
import {
  FetchOneClickDeploymentStateLogSubscriptionSubscription,
  One_Click_Deployment_States_Enum as OneClickDeploymentState,
} from '../../ControlPlane';

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

type RequiredEnvVarKind =
  | 'ENV_TYPE_STATIC'
  | 'ENV_TYPE_DYNAMIC'
  | 'ENV_TYPE_DATABASE';

type RequiredEnvVarValueType = 'STRING_ARRAY' | 'NUMBER' | 'JSON' | 'TEXT';

export type RequiredEnvVar = {
  Kind: RequiredEnvVarKind;
  Name: string;
  Default?: string;
  SubKind?: Driver;
  Mandatory?: boolean;
  Description?: string;
  Position?: number;
  ValueType?: RequiredEnvVarValueType;
  Placeholder?: string;
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
      logId: number;
    }
  | {
      kind: 'awaiting';
      payload: RequiredEnvVar[] | { envs: RequiredEnvVar[] };
    }
  | {
      kind: 'success';
    }
  | {
      kind: 'idle';
    };

export type ProgressState = Record<UserFacingStep, ProgressStateStatus>;

export type EnvVarsFormState = 'default' | 'loading' | 'error' | 'hidden';

export type GitRepoDetails = {
  url: string;
  branch?: string;
  hasuraDirectory?: string;
};

export type FallbackApp = {
  name: string;
  react_icons_component_name: string;
  href: string;
};
