import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { GraphiqlPopup } from './GraphiqlPopup';

export default {
  title: 'features/CloudOnboarding/One Click Deployment/Graphiql Popup',
  component: GraphiqlPopup,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof GraphiqlPopup>;

export const Default: Story = () => (
  <GraphiqlPopup
    status="success"
    gitRepoName="abhi40308/hasura-sample-app-ocd"
    gitRepoFullLink="https://github.com/abhi40308/hasura-sample-app-ocd/tree/main/hasura"
    dismissCb={() => {}}
  />
);

export const Error: Story = () => {
  return (
    <GraphiqlPopup
      status="error"
      gitRepoName="abhi40308/hasura-sample-app-ocd"
      gitRepoFullLink="https://github.com/abhi40308/hasura-sample-app-ocd/tree/main/hasura"
      dismissCb={() => {}}
    />
  );
};
