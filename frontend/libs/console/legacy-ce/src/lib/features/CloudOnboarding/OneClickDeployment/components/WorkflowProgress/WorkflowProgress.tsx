import React from 'react';
import { useAppDispatch } from '@/store';
import { programmaticallyTraceError } from '@/features/Analytics';
import {
  FetchOneClickDeploymentStateLogSubscriptionSubscription,
  FetchOneClickDeploymentStateLogSubscriptionSubscriptionVariables,
  FETCH_ONE_CLICK_DEPLOYMENT_STATE_LOG_SUBSCRIPTION,
  controlPlaneClient,
} from '@/features/ControlPlane';
import { reactQueryClient } from '@/lib/reactQuery';
import {
  forceChangeGraphiqlQuery,
  forceGraphiQLIntrospection,
} from '../../../../../components/Services/ApiExplorer/OneGraphExplorer/utils';
import {
  RequiredEnvVar,
  OneClickDeploymentState,
  ProgressState,
  EnvVarsFormState,
} from '../../types';
import { getCliProgressState, shouldTriggerFirstDeployment } from '../../util';
import { CliScreen } from '../CliScreen/CliScreen';
import { RedirectCountDown } from '../RedirectCountdown/RedirectCountDown';
import { EnvVarsForm } from '../EnvVarsForm/EnvVarsForm';
import { useTriggerDeployment } from '../../hooks';
import { getTenantEnvVarsQueryKey } from '../../constants';

type WorkflowProgressProps = {
  setStepperIndex: React.Dispatch<React.SetStateAction<number>>;
  deploymentId: number;
  projectId: string;
  onCompleteSuccess?: VoidFunction;
  onCompleteError?: VoidFunction;
};

export function WorkflowProgress(props: WorkflowProgressProps) {
  const { projectId, setStepperIndex, onCompleteSuccess, onCompleteError } =
    props;
  const dispatch = useAppDispatch();

  const [progressState, setProgressState] = React.useState<ProgressState>(
    getCliProgressState([])
  );
  const [requiredEnvVars, setRequiredEnvVars] = React.useState<
    RequiredEnvVar[]
  >([]);
  const [envVarsformState, setEnvVarsFormState] =
    React.useState<EnvVarsFormState>('hidden');

  // function to trigger deployment for the first time
  // and to retrigger deployment after the env vars have been set
  const { triggerDeployment } = useTriggerDeployment(projectId);

  // set required env vars in state when the workflow is waiting for env vars
  React.useEffect(() => {
    const awaitingStepStatus =
      progressState[OneClickDeploymentState.AwaitingEnvironmentVariables];
    if (awaitingStepStatus.kind === 'awaiting') {
      // refetch queries every time deployment goes into awaiting state,
      // overriding the stale time
      reactQueryClient.refetchQueries(getTenantEnvVarsQueryKey);
      setRequiredEnvVars(awaitingStepStatus.payload);
      setEnvVarsFormState('default');
    }
    const sufficientEnvStepStatus =
      progressState[OneClickDeploymentState.SufficientEnvironmentVariables];
    if (sufficientEnvStepStatus.kind === 'success') {
      setEnvVarsFormState('hidden');
    }
  }, [progressState]);

  // set appropriate stepper index on checkpoints
  React.useEffect(() => {
    // Move the stepper to `Get started` if the deployment is complete
    if (
      progressState[OneClickDeploymentState.ApplyingMetadataMigrationsSeeds]
        .kind === 'success'
    ) {
      // trigger introspection in graphiql after the deployment is successful
      // so that graphiql is ready to use when onboarding is dismissed
      forceGraphiQLIntrospection(dispatch);

      // this timeout makes sure that there's a delay in setting query after introspection has been fired
      // this timeout does not intend to wait for introspection to finish
      setTimeout(() => {
        // change the query to empty string, to remove the default comment(for empty new projects) in graphiQL
        forceChangeGraphiqlQuery('', dispatch);
      }, 500);
      setStepperIndex(3);
      return;
    }

    // Move the stepper to `Deploy` if there are sufficient env vars
    if (
      progressState[OneClickDeploymentState.SufficientEnvironmentVariables]
        .kind === 'success'
    ) {
      setStepperIndex(2);
      return;
    }

    setStepperIndex(1);
  }, [progressState]);

  // subscribe to the one click deployment workflow state in the database
  React.useEffect(() => {
    // TODO: 1) Loading state 2) Error state
    const { unsubscribe } = controlPlaneClient.subscribe<
      FetchOneClickDeploymentStateLogSubscriptionSubscription,
      FetchOneClickDeploymentStateLogSubscriptionSubscriptionVariables
    >(
      FETCH_ONE_CLICK_DEPLOYMENT_STATE_LOG_SUBSCRIPTION,
      {
        id: props.deploymentId,
      },
      data => {
        const oneClickDeploy = data.one_click_deployment_by_pk;
        if (!oneClickDeploy) {
          unsubscribe();
          // If the deployment is not found, move to the graphiql popup stage with an
          // error message on the popup.
          if (onCompleteError) {
            onCompleteError();
          }
          return;
        }

        // if the deployment workflow hasn't started, trigger it
        if (
          shouldTriggerFirstDeployment(
            oneClickDeploy.one_click_deployment_state_logs
          )
        ) {
          triggerDeployment();
        }

        setProgressState(
          getCliProgressState(oneClickDeploy.one_click_deployment_state_logs)
        );
      },
      error => {
        programmaticallyTraceError(
          new Error('failed subscribing to one click deployment status'),
          {
            errorMessage: error.message,
            sourceError: error,
          },
          'error'
        );
      }
    );
    return () => {
      unsubscribe();
    };
  }, []);

  return (
    <div className="w-full">
      <CliScreen state={progressState} triggerDeployment={triggerDeployment} />
      {progressState[OneClickDeploymentState.Completed].kind === 'success' && (
        <RedirectCountDown
          timeSeconds={5}
          redirect={() => {
            if (onCompleteSuccess) {
              onCompleteSuccess();
            }
          }}
        />
      )}
      {envVarsformState !== 'hidden' && (
        <EnvVarsForm
          envVars={requiredEnvVars}
          formState={envVarsformState}
          setFormState={setEnvVarsFormState}
          successCb={() => {
            triggerDeployment();
          }}
        />
      )}
    </div>
  );
}
