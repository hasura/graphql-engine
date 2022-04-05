import React from 'react';
import * as z from 'zod';
import { Story, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { Form } from '@/new-components/Form';
import { Button } from '@/new-components/Button';
import { handlers } from '../../__mocks__';
import { RemoteDatabaseWidget } from './RemoteDatabaseWidget';

const defaultValues = {
  database: '',
  schema: '',
  table: '',
};

export default {
  title: 'Remote Relationships/Components/Remote Database Widget',
  component: RemoteDatabaseWidget,
  decorators: [
    ReactQueryDecorator(),
    StoryComponent => (
      <Form
        schema={z.any()}
        onSubmit={o => console.log(o)}
        options={{ defaultValues }}
      >
        {() => (
          <div>
            <StoryComponent />
            <Button type="submit">Submit</Button>
          </div>
        )}
      </Form>
    ),
  ],
  parameters: {
    msw: handlers(),
  },
} as Meta;

export const Primary: Story = args => <RemoteDatabaseWidget {...args} />;
Primary.args = {};
Primary.parameters = {
  // Disable chromatic snapshot for playground stories
  chromatic: { disableSnapshot: true },
};
