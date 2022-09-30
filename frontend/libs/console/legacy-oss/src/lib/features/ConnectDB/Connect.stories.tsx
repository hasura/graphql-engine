import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';

import { ComponentStory, ComponentMeta } from '@storybook/react';
import { Connect } from './Connect';

import { handlers } from './mocks/handlers.mock';

export default {
  title: 'Data/Connect',
  component: Connect.CreateConnection,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as ComponentMeta<typeof Connect.CreateConnection>;

export const Primary: ComponentStory<typeof Connect.CreateConnection> = () => (
  <Connect.CreateConnection
    name="new_connection"
    driver="postgres"
    onDriverChange={() => {}}
  />
);

// existing config currently only works with database url
// we don't know what format the metadata will be returned for gdc yet
// therefore editing exiting config won't be enabled for gdc on the first iteration anyway
export const WithExistingConfig: ComponentStory<
  typeof Connect.CreateConnection
> = () => (
  <Connect.CreateConnection
    name="default"
    driver="postgres"
    onDriverChange={() => {}}
  />
);
