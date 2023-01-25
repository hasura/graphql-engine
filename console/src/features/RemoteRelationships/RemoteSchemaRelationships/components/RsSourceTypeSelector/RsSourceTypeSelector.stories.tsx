import React from 'react';
import * as z from 'zod';
import { action } from '@storybook/addon-actions';
import { Meta, Story } from '@storybook/react';
import { SimpleForm } from '@/new-components/Form';
import { Button } from '@/new-components/Button';
import {
  remoteSchemaSelectorKey,
  RsSourceTypeSelector,
  RsSourceTypeSelectorProps,
} from './RsSourceTypeSelector';

const defaultValues = {
  [remoteSchemaSelectorKey]: 'remoteSchema2',
};

export default {
  title:
    'Features/Remote Relationships/Components/Remote Schema Source Type Selector',
  component: RsSourceTypeSelector,
  decorators: [
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
