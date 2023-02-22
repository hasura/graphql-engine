import React from 'react';
import { Meta, Story } from '@storybook/react';
import { z } from 'zod';
import { SimpleForm } from '../../../../new-components/Form';
import { action } from '@storybook/addon-actions';

import { AggregationProps, AggregationSection } from './Aggregation';

export default {
  title: 'Features/Permissions/Form/Aggregation Section',
  component: AggregationSection,
  parameters: {
    // Disable storybook for playground stories
    chromatic: { disableSnapshot: true },
  },
} as Meta;

const schema = z.object({
  enableAggregation: z.boolean(),
});

export const AggregationEnabled: Story<AggregationProps> = args => (
  <AggregationSection {...args} />
);
AggregationEnabled.args = {
  roleName: 'one',
};
AggregationEnabled.decorators = [
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
];

export const AggregationDisabled: Story<AggregationProps> = args => (
  <AggregationSection {...args} />
);
AggregationDisabled.args = {
  roleName: 'two',
};
AggregationDisabled.decorators = [
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
];

export const Showcase: Story<AggregationProps> = () => (
  <AggregationSection queryType="insert" roleName="one" defaultOpen />
);
Showcase.parameters = {
  ...AggregationEnabled.args,
  defaultOpen: true,
};
Showcase.decorators = AggregationEnabled.decorators;
Showcase.parameters = {
  // Disable storybook for playground stories
  chromatic: { disableSnapshot: false },
};
