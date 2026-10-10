import * as React from 'react';
import { FaChevronRight } from 'react-icons/fa';
import {
  OneClickDeploymentState,
  UserFacingStep,
  ProgressStateStatus,
  FallbackApp,
} from '../../types';
import { StatusIcon } from './components/StatusIcon';
import { getStepText } from './utils';
import { ErrorBox } from './components/ErrorBox';
import { Flex, Progress } from '@radix-ui/themes';
import { Text } from '@hasura/shared/ui';

type Props = {
  step: UserFacingStep;
  status: ProgressStateStatus;
  retryAction: VoidFunction;
  fallbackApps: FallbackApp[];
};

export const CliLog: React.FC<Props> = (props) => {
  const { step, status, retryAction, fallbackApps } = props;

  const showProgressBar =
    step === OneClickDeploymentState.ApplyingMetadataMigrationsSeeds &&
    status.kind !== 'success' &&
    status.kind !== 'error';

  // only show steps that are relevent to the users
  // i.e. hide internal util steps

  // hide idle steps
  if (status.kind === 'idle') {
    return null;
  }

  switch (step) {
    case OneClickDeploymentState.SufficientEnvironmentVariables:
      return null;
    default:
      const log = (
        <div className="mt-4">
          {showProgressBar ? (
            <Progress
              aria-label="Applying metadata, migrations and seeds"
              color="tomato"
              radius="none"
              size="1"
              className="fixed! top-0 left-0 z-[9999] h-0.5! w-full"
            />
          ) : null}
          <Flex align="center" key={step} className="mt-2">
            <FaChevronRight />
            <StatusIcon step={step} status={status} />
            <Text className="tracking-widest">{getStepText(step, status)}</Text>
          </Flex>
        </div>
      );
      if (status.kind === 'error') {
        return (
          <Flex direction="column" className="w-full mt-4">
            {log}
            <div className="w-full mt-2">
              <ErrorBox
                step={step}
                error={status.error}
                logId={status.logId}
                retryAction={retryAction}
                fallbackApps={fallbackApps}
              />
            </div>
          </Flex>
        );
      }
      return log;
  }
};
