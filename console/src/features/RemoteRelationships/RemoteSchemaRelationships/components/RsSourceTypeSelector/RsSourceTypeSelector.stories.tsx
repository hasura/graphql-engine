import React from 'react';
import * as z from 'zod';
import { Story, Meta } from '@storybook/react';
import { Form } from '@/new-components/Form';
import { Button } from '@/new-components/Button';
import {
  RsSourceTypeSelector,
  RsSourceTypeSelectorProps,
  remoteSchemaSelectorKey,
} from './RsSourceTypeSelector';

const defaultValues = {
  [remoteSchemaSelectorKey]: 'remoteSchema2',
};

export default {
  title: 'Remote Relationships/Components/Remote Schema Source Type Selector',
  component: RsSourceTypeSelector,
  decorators: [
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
} as Meta;

export const Primary: Story<RsSourceTypeSelectorProps> = args => (
  <RsSourceTypeSelector {...args} />
);
Primary.args = {
  types: ['country', 'continent', 'language', 'state'],
  sourceTypeKey: 'type_name',
};
Primary.parameters = {
  // Disable chromatic snapshot for playground stories
  chromatic: { disableSnapshot: true },
};
