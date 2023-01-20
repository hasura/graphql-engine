import * as React from 'react';
import { ProgressState, UserFacingStep } from '../../types';
import { CliLog } from '../CliLog';

type Props = {
  state: ProgressState;
  triggerDeployment: VoidFunction;
};

export const CliScreen: React.VFC<Props> = props => {
  const { state, triggerDeployment } = props;
  return (
    <div className="bg-[#0F172A] h-[376px]">
      <div className="p-md pb-xl overflow-auto">
        {Object.keys(state).map((key, index) => {
          const step = key as UserFacingStep;
          const status = state[step];
          return (
            <CliLog
              key={index}
              step={step}
              status={status}
              retryAction={triggerDeployment}
            />
          );
        })}
      </div>
    </div>
  );
};
