import React from 'react';
import { programmaticallyTraceError } from '@hasura/shared/analytics';
import {
  FetchOneClickDeploymentStateLogSubscriptionSubscription,
  FetchOneClickDeploymentStateLogSubscriptionSubscriptionVariables,
  FETCH_ONE_CLICK_DEPLOYMENT_STATE_LOG_SUBSCRIPTION,
  controlPlaneClient,
} from '../../../../ControlPlane';
import {
  RequiredEnvVar,
  OneClickDeploymentState,
  ProgressState,
  EnvVarsFormState,
  FallbackApp,
} from '../../types';
import {
  getCliProgressState,
  isHasuraPath,
  shouldTriggerFirstDeployment,
} from '../../util';
import { CliScreen } from '../CliScreen/CliScreen';
import { RedirectCountDown } from '../RedirectCountdown/RedirectCountDown';
import { Disclaimer } from '../Disclaimer/Disclaimer';
import { EnvVarsForm } from '../EnvVarsForm/EnvVarsForm';
import { useTriggerDeployment, useSampleQuery } from '../../hooks';
import { getTenantEnvVarsQueryKey } from '../../constants';
import { useQueryClient } from '@tanstack/react-query';

type WorkflowProgressProps = {
  setStepperIndex: React.Dispatch<React.SetStateAction<number>>;
  deploymentId: string;
  projectId: string;
  gitRepoName: string;
  onCompleteSuccess?: VoidFunction;
  sampleQueriesFileUrl: string;
  onCompleteError?: VoidFunction;
  fallbackApps: FallbackApp[];
};

export function WorkflowProgress(props: WorkflowProgressProps) {
  const {
    projectId,
    setStepperIndex,
    gitRepoName,
    onCompleteSuccess,
    onCompleteError,
    fallbackApps,
    sampleQueriesFileUrl,
  } = props;
  const queryClient = useQueryClient();
  const [progressState, setProgressState] = React.useState<ProgressState>(
    getCliProgressState([]),
  );
  const [requiredEnvVars, setRequiredEnvVars] = React.useState<
    RequiredEnvVar[]
  >([]);
  const [envVarsformState, setEnvVarsFormState] =
    React.useState<EnvVarsFormState>('hidden');

  // function to trigger deployment for the first time
  // and to retrigger deployment after the env vars have been set
  const { triggerDeployment } = useTriggerDeployment(projectId);

  // Derive the env-vars form state from `progressState` when it changes.
  // `envVarsformState` and `requiredEnvVars` are also written by the form itself
  // (e.g. 'loading'/'error' on submit), so they cannot be purely derived; we use
  // React's "adjusting state while a value changes" pattern (set during render,
  // guarded by a previous-value comparison) instead of a setState-in-effect,
  // which would trigger an extra render pass.
  const [prevProgressState, setPrevProgressState] =
    React.useState(progressState);
  if (progressState !== prevProgressState) {
    setPrevProgressState(progressState);

    const awaitingStepStatus =
      progressState[OneClickDeploymentState.AwaitingEnvironmentVariables];
    if (awaitingStepStatus.kind === 'awaiting') {
      if ('envs' in awaitingStepStatus.payload) {
        setRequiredEnvVars(awaitingStepStatus.payload.envs);
      } else if ('length' in awaitingStepStatus.payload) {
        setRequiredEnvVars(awaitingStepStatus.payload);
      }

      setEnvVarsFormState('default');
    } else {
      setEnvVarsFormState('hidden');
    }
  }

  // refetch queries every time deployment goes into awaiting state, overriding
  // the stale time. This is a genuine side effect, so it stays in an effect.
  React.useEffect(() => {
    const awaitingStepStatus =
      progressState[OneClickDeploymentState.AwaitingEnvironmentVariables];
    if (awaitingStepStatus.kind === 'awaiting') {
      queryClient.refetchQueries({
        queryKey: [getTenantEnvVarsQueryKey],
      });
    }
  }, [progressState, queryClient]);

  // get the sample query associated with this deployment to prefill in graphiql
  const query = useSampleQuery(sampleQueriesFileUrl);

  // set appropriate stepper index on checkpoints
  React.useEffect(() => {
    // Move the stepper to `Get started` if the deployment is complete
    if (
      progressState[OneClickDeploymentState.ApplyingMetadataMigrationsSeeds]
        .kind === 'success'
    ) {
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
  }, [progressState, query, setStepperIndex]);

  // The subscription must be set up exactly once on mount (and torn down on
  // unmount). `deploymentId`, `onCompleteError` and `triggerDeployment` are
  // read from the live subscription callback but must not re-create the
  // subscription, so we keep their latest values in refs.
  const deploymentIdRef = React.useRef(props.deploymentId);
  const onCompleteErrorRef = React.useRef(onCompleteError);
  const triggerDeploymentRef = React.useRef(triggerDeployment);
  React.useEffect(() => {
    deploymentIdRef.current = props.deploymentId;
    onCompleteErrorRef.current = onCompleteError;
    triggerDeploymentRef.current = triggerDeployment;
  });

  // subscribe to the one click deployment workflow state in the database
  React.useEffect(() => {
    // TODO: 1) Loading state 2) Error state
    const { unsubscribe } = controlPlaneClient.subscribe<
      FetchOneClickDeploymentStateLogSubscriptionSubscription,
      FetchOneClickDeploymentStateLogSubscriptionSubscriptionVariables
    >(
      FETCH_ONE_CLICK_DEPLOYMENT_STATE_LOG_SUBSCRIPTION,
      {
        id: deploymentIdRef.current,
      },
      (data) => {
        const oneClickDeploy = data.one_click_deployment_by_pk;
        if (!oneClickDeploy) {
          unsubscribe();
          // If the deployment is not found, move to the graphiql popup stage with an
          // error message on the popup.
          if (onCompleteErrorRef.current) {
            onCompleteErrorRef.current();
          }
          return;
        }

        // if the deployment workflow hasn't started, trigger it
        if (
          shouldTriggerFirstDeployment(
            oneClickDeploy.one_click_deployment_state_logs,
          )
        ) {
          triggerDeploymentRef.current();
        }

        setProgressState(
          getCliProgressState(oneClickDeploy.one_click_deployment_state_logs),
        );
      },
      (error) => {
        programmaticallyTraceError({
          error: 'failed subscribing to one click deployment status',
          cause: error,
        });
      },
    );
    return () => {
      unsubscribe();
    };
  }, []);

  return (
    <div className="w-full">
      <CliScreen
        state={progressState}
        triggerDeployment={triggerDeployment}
        fallbackApps={fallbackApps}
      />
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
      {!isHasuraPath(gitRepoName) ? (
        <div className="mt-2 w-full">
          <Disclaimer />
        </div>
      ) : null}
    </div>
  );
}
