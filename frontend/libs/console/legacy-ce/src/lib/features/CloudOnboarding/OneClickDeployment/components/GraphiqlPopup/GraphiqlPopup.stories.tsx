import React from 'react';
import { StoryFn, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { GraphiqlPopup } from './GraphiqlPopup';

export default {
  title: 'features/CloudOnboarding/One Click Deployment/Graphiql Popup',
  component: GraphiqlPopup,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof GraphiqlPopup>;

export const Default: StoryFn = () => (
  <GraphiqlPopup
    status="success"
    gitRepoName="abhi40308/hasura-sample-app-ocd"
    gitRepoFullLink="https://github.com/abhi40308/hasura-sample-app-ocd/tree/main/hasura"
    dismissCb={() => {}}
  />
);

export const Error: StoryFn = () => {
  return (
    <GraphiqlPopup
      status="error"
      gitRepoName="abhi40308/hasura-sample-app-ocd"
      gitRepoFullLink="https://github.com/abhi40308/hasura-sample-app-ocd/tree/main/hasura"
      dismissCb={() => {}}
    />
  );
};
