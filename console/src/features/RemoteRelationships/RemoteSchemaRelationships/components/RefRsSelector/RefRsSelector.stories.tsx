import React from 'react';
import * as z from 'zod';
import { Story, Meta } from '@storybook/react';
import { Form } from '@/new-components/Form';
import {
  RefRsSelector,
  RefRsSelectorProps,
  refRemoteSchemaSelectorKey,
} from './RefRsSelector';

const defaultValues = {
  [refRemoteSchemaSelectorKey]: 'pokemon',
};

export default {
  title:
    'Features/Remote Relationships/Components/Reference Remote Schema Selector',
  component: RefRsSelector,
  decorators: [
    StoryComponent => (
      <Form
        schema={z.any()}
        onSubmit={o => console.log(o)}
        options={{ defaultValues }}
      >
        {() => <StoryComponent />}
      </Form>
    ),
  ],
} as Meta;

export const Primary: Story<RefRsSelectorProps> = args => (
  <RefRsSelector {...args} />
);
Primary.args = {
  allRemoteSchemas: ['rs1', 'rs2', 'pokemon', 'countries'],
};
Primary.parameters = {
  // Disable chromatic snapshot for playground stories
  chromatic: { disableSnapshot: true },
};
