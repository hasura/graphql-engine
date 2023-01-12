import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { CliScreen } from './CliScreen';
import { OneClickDeploymentState } from '../../types';

export default {
  title: 'features/CloudOnboarding/One Click Deployment/Cli Screen',
  component: CliScreen,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof CliScreen>;

export const Happy: Story = () => (
  <CliScreen
    state={{
      [OneClickDeploymentState.Initialized]: { kind: 'success' },
      [OneClickDeploymentState.CloningGitRepository]: { kind: 'success' },
      [OneClickDeploymentState.ReadingEnvironmentVariables]: {
        kind: 'success',
      },
      [OneClickDeploymentState.AwaitingEnvironmentVariables]: { kind: 'idle' },
      [OneClickDeploymentState.SufficientEnvironmentVariables]: {
        kind: 'success',
      },
      [OneClickDeploymentState.ApplyingMetadataMigrationsSeeds]: {
        kind: 'success',
      },
      [OneClickDeploymentState.Completed]: { kind: 'success' },
    }}
    triggerDeployment={() => {}}
  />
);

export const Error: Story = () => {
  return (
    <CliScreen
      state={{
        [OneClickDeploymentState.Initialized]: { kind: 'success' },
        [OneClickDeploymentState.CloningGitRepository]: { kind: 'success' },
        [OneClickDeploymentState.ReadingEnvironmentVariables]: {
          kind: 'success',
        },
        [OneClickDeploymentState.AwaitingEnvironmentVariables]: {
          kind: 'idle',
        },
        [OneClickDeploymentState.SufficientEnvironmentVariables]: {
          kind: 'success',
        },
        [OneClickDeploymentState.ApplyingMetadataMigrationsSeeds]: {
          kind: 'error',
          error: { message: 'unable to connect' },
        },
        [OneClickDeploymentState.Completed]: { kind: 'idle' },
      }}
      triggerDeployment={() => {}}
    />
  );
};
