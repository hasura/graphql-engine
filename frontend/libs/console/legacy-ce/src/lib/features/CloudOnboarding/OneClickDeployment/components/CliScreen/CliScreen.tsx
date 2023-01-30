import * as React from 'react';
import { ProgressState, UserFacingStep, FallbackApp } from '../../types';
import { CliLog } from '../CliLog';

type Props = {
  state: ProgressState;
  triggerDeployment: VoidFunction;
  fallbackApps: FallbackApp[];
};

export const CliScreen: React.VFC<Props> = props => {
  const { state, triggerDeployment, fallbackApps } = props;
  return (
    <div className="font-mono overflow-auto bg-[#0F172A] h-[30rem]">
      <div className="p-md pb-xl">
        {Object.keys(state).map((key, index) => {
          const step = key as UserFacingStep;
          const status = state[step];
          return (
            <CliLog
              key={index}
              step={step}
              status={status}
              retryAction={triggerDeployment}
              fallbackApps={fallbackApps}
            />
          );
        })}
      </div>
    </div>
  );
};
