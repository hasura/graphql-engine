import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { z } from 'zod';
import { SimpleForm } from '../../../../new-components/Form';
import { action } from '@storybook/addon-actions';

import { AggregationProps, AggregationSection } from './Aggregation';

export default {
  component: AggregationSection,
  parameters: {
    // Disable storybook for playground stories
    chromatic: { disableSnapshot: true },
  },
} as Meta;

const schema = z.object({
  enableAggregation: z.boolean(),
});

export const AggregationEnabled: StoryObj<AggregationProps> = {
  args: {
    roleName: 'one',
  },

  decorators: [
    (StoryComponent: React.FC) => (
      <SimpleForm
        schema={schema}
        onSubmit={action('onSubmit')}
        options={{ defaultValues: { enableAggregation: true } }}
        className="p-4"
      >
        <StoryComponent />
      </SimpleForm>
    ),
  ],
};

export const AggregationDisabled: StoryObj<AggregationProps> = {
  args: {
    roleName: 'two',
  },

  decorators: [
    (StoryComponent: React.FC) => (
      <SimpleForm
        schema={schema}
        onSubmit={action('onSubmit')}
        options={{ defaultValues: { enableAggregation: false } }}
        className="p-4"
      >
        <StoryComponent />
      </SimpleForm>
    ),
  ],
};

export const Showcase: StoryObj<AggregationProps> = {
  render: () => (
    <AggregationSection queryType="insert" roleName="one" defaultOpen />
  ),

  parameters: {
    // Disable storybook for playground stories
    chromatic: { disableSnapshot: false },
  },

  decorators: AggregationEnabled.decorators,
};
