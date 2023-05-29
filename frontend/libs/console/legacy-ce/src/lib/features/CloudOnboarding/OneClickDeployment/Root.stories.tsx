import React from 'react';
import { StoryFn, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { Root } from './Root';

export default {
  title: 'features/CloudOnboarding/One Click Deployment/Root',
  component: Root,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof Root>;

export const Base: StoryFn = () => {
  return (
    <Root
      deployment={{
        deploymentId: 1234,
        gitRepoDetails: {
          url: 'https://github.com/abhi40308/hasura-sample-app-ocd',
        },
      }}
      dismissOnboarding={() => null}
      fallbackApps={[]}
    />
  );
};
