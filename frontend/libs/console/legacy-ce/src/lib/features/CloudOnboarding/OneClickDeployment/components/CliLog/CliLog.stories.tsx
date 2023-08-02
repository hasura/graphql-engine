import React from 'react';
import { StoryFn, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { CliLog } from './CliLog';
import { OneClickDeploymentState } from '../../types';

export default {
  title: 'features/CloudOnboarding/One Click Deployment/CLILog',
  component: CliLog,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof CliLog>;

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

const StoryWrapper: React.FC = ({ children }) => {
  return (
    <div className="p-md w-auto bg-black rounded items-center">{children}</div>
  );
};

export const Initializing: StoryFn = () => (
  <StoryWrapper>
    <CliLog
      step={OneClickDeploymentState.Initialized}
      status={{
        kind: 'in-progress',
      }}
      retryAction={() => {}}
      fallbackApps={fallbackApps}
    />
  </StoryWrapper>
);

export const Initialized: StoryFn = () => (
  <StoryWrapper>
    <CliLog
      step={OneClickDeploymentState.Initialized}
      status={{
        kind: 'success',
      }}
      retryAction={() => {}}
      fallbackApps={fallbackApps}
    />
  </StoryWrapper>
);

export const ErrorLogWithExpectedError: StoryFn = () => (
  <StoryWrapper>
    <CliLog
      step={OneClickDeploymentState.Initialized}
      status={{
        kind: 'error',
        error: { error: { message: 'something went wrong' } },
      }}
      retryAction={() => {}}
      fallbackApps={fallbackApps}
    />
  </StoryWrapper>
);

export const ErrorLogWithUnknownError: StoryFn = () => (
  <StoryWrapper>
    <CliLog
      step={OneClickDeploymentState.Initialized}
      status={{
        kind: 'error',
        error: { message: 'something went wrong' },
      }}
      retryAction={() => {}}
      fallbackApps={fallbackApps}
    />
  </StoryWrapper>
);
