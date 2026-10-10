import React from 'react';
import { ReactQueryDecorator } from '@hasura/shared/testing';

import { StoryFn, Meta } from '@storybook/react-webpack5';
import { Connect } from './Connect';

import { handlers } from './mocks/handlers.mock';

export default {
  title: 'Data/Connect',
  component: Connect.CreateConnection,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta<typeof Connect.CreateConnection>;

export const Primary: StoryFn<typeof Connect.CreateConnection> = () => (
  <Connect.CreateConnection
    name="new_connection"
    driver="postgres"
    onDriverChange={() => {}}
  />
);

export const WithExistingConfig: StoryFn<
  typeof Connect.CreateConnection
> = () => (
  <Connect.CreateConnection
    name="default"
    driver="postgres"
    onDriverChange={() => {}}
  />
);
