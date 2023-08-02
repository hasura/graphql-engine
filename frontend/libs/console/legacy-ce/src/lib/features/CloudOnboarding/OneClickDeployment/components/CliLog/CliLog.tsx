import * as React from 'react';
import ProgressBar from 'react-progress-bar-plus';
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

type Props = {
  step: UserFacingStep;
  status: ProgressStateStatus;
  retryAction: VoidFunction;
  fallbackApps: FallbackApp[];
};

export const CliLog: React.VFC<Props> = props => {
  const { step, status, retryAction, fallbackApps } = props;

  const [progressBarPercent, setProgressBarPercent] =
    React.useState<number>(-1);

  React.useEffect(() => {
    if (step === OneClickDeploymentState.ApplyingMetadataMigrationsSeeds) {
      if (status.kind === 'success' || status.kind === 'error') {
        setProgressBarPercent(100);
      } else {
        setProgressBarPercent(10);
      }
    }
  }, [step, status.kind]);

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
        <>
          <ProgressBar
            percent={progressBarPercent}
            autoIncrement
            spinner={false}
          />
          <div className="flex items-center" key={step}>
            <FaChevronRight className="mr-xs mt-xs text-slate-400" />
            <div className="mt-xs mr-xs">
              <StatusIcon step={step} status={status} />
            </div>
            <div className="mt-xs text-slate-50 tracking-widest">
              {getStepText(step, status)}
            </div>
          </div>
        </>
      );
      if (status.kind === 'error') {
        return (
          <div className="flex w-full flex-col">
            {log}
            <div className="w-full mt-xs">
              <ErrorBox
                step={step}
                error={status.error}
                logId={status.logId}
                retryAction={retryAction}
                fallbackApps={fallbackApps}
              />
            </div>
          </div>
        );
      }
      return log;
  }
};
