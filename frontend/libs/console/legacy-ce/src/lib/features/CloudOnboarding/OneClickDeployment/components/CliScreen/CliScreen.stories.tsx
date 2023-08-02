import React from 'react';
import { StoryFn, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { CliScreen } from './CliScreen';
import { OneClickDeploymentState } from '../../types';

export default {
  title: 'features/CloudOnboarding/One Click Deployment/Cli Screen',
  component: CliScreen,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof CliScreen>;

const fallbackApps = [
  {
    name: 'Ecommerce',
    react_icons_component_name: 'FaShoppingCart',
    href: '/deploy?github_repo=https://github.com/hasura/data-hub&hasura_dir=data-models/postgres/ecommerce/hasura&branch=main',
  },
  {
    name: 'Chinook',
    react_icons_component_name: 'FaMusic',
    href: '/deploy?github_repo=https://github.com/hasura/data-hub&hasura_dir=data-models/postgres/chinook/hasura&branch=main',
  },
  {
    name: 'Sakila',
    react_icons_component_name: 'FaCompactDisc',
    href: '/deploy?github_repo=https://github.com/hasura/data-hub&hasura_dir=data-models/postgres/sakila/hasura&branch=main',
  },
];

export const Happy: StoryFn = () => (
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
    fallbackApps={fallbackApps}
  />
);

export const Error: StoryFn = () => {
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
      fallbackApps={fallbackApps}
    />
  );
};
