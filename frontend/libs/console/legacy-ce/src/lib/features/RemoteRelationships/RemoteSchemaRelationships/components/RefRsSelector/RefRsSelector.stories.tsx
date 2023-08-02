import React from 'react';
import * as z from 'zod';
import { StoryObj, Meta } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { SimpleForm } from '../../../../../new-components/Form';
import {
  refRemoteSchemaSelectorKey,
  RefRsSelector,
  RefRsSelectorProps,
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
      <SimpleForm
        schema={z.any()}
        onSubmit={action('onSubmit')}
        options={{ defaultValues }}
        className="p-4"
      >
        <StoryComponent />
      </SimpleForm>
    ),
  ],
} as Meta;

export const Primary: StoryObj<RefRsSelectorProps> = {
  args: {
    allRemoteSchemas: ['rs1', 'rs2', 'pokemon', 'countries'],
  },

  parameters: {
    // Disable chromatic snapshot for playground stories
    chromatic: { disableSnapshot: true },
  },
};
