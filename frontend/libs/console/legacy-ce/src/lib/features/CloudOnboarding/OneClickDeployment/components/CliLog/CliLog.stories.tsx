import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { CliLog } from './CliLog';
import { OneClickDeploymentState } from '../../types';

export default {
  title: 'features/CloudOnboarding/One Click Deployment/CLILog',
  component: CliLog,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof CliLog>;

const StoryWrapper: React.FC = ({ children }) => {
  return (
    <div className="p-md w-auto bg-black rounded items-center">{children}</div>
  );
};

export const Initialising: Story = () => (
  <StoryWrapper>
    <CliLog
      step={OneClickDeploymentState.Initialized}
      status={{
        kind: 'in-progress',
      }}
      retryAction={() => {}}
    />
  </StoryWrapper>
);

export const Initialised: Story = () => (
  <StoryWrapper>
    <CliLog
      step={OneClickDeploymentState.Initialized}
      status={{
        kind: 'success',
      }}
      retryAction={() => {}}
    />
  </StoryWrapper>
);

export const InitialisationError: Story = () => (
  <StoryWrapper>
    <CliLog
      step={OneClickDeploymentState.Initialized}
      status={{
        kind: 'error',
        error: { message: 'something went wrong' },
      }}
      retryAction={() => {}}
    />
  </StoryWrapper>
);
