import React from 'react';
import { Story, Meta } from '@storybook/react';
import { z } from 'zod';
import { Form } from '@/new-components/Form';

import { AggregationSection, AggregationProps } from './Aggregation';

export default {
  title: 'Features/Permissions Form/Components/Aggregation Section',
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
    <Form
      schema={schema}
      onSubmit={() => {}}
      options={{ defaultValues: { enableAggregation: true } }}
    >
      {() => <StoryComponent />}
    </Form>
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
    <Form
      schema={schema}
      onSubmit={() => {}}
      options={{ defaultValues: { enableAggregation: false } }}
    >
      {() => <StoryComponent />}
    </Form>
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
