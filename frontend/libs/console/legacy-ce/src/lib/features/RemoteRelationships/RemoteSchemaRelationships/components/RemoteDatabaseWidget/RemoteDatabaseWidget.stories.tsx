import React from 'react';
import * as z from 'zod';
import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { action } from '@storybook/addon-actions';
import { SimpleForm } from '../../../../../new-components/Form';
import { Button } from '../../../../../new-components/Button';
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
    StoryComponent => (
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
