import React from 'react';
import * as z from 'zod';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { ReactQueryDecorator } from '@hasura/shared/testing';
import { action } from 'storybook/actions';
import { SimpleForm, Button } from '@hasura/shared/ui';

import { handlers } from '../../__mocks__';
import { RemoteDatabaseWidget } from './RemoteDatabaseWidget';

const defaultValues = {
  database: '',
  schema: '',
  table: '',
  driver: '',
};

export default {
  title: 'Features/Remote Relationships/Components/Remote Database Widget',
  component: RemoteDatabaseWidget,
  decorators: [
    ReactQueryDecorator(),
    (StoryComponent) => (
      <SimpleForm
        schema={z.any()}
        onSubmit={action('onSubmit')}
        options={{ defaultValues }}
        className="p-4"
      >
        <div>
          <StoryComponent />
          <Button type="submit">Submit</Button>
        </div>
      </SimpleForm>
    ),
  ],
  parameters: {
    msw: handlers(),
  },
} as Meta;

export const Primary: StoryObj = {
  args: {},

  parameters: {
    // Disable chromatic snapshot for playground stories
    chromatic: { disableSnapshot: true },
  },
};
