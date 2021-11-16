import React from 'react';
import { Story, Meta } from '@storybook/react';
import { z } from 'zod';
import { Form } from '@/new-components/Form';

import { AggregationSection, AggregationProps } from './Aggregation';

export default {
  title: 'Permissions Form/Components/Aggregation Section',
  component: AggregationSection,
  decorators: [],
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
  (S: React.FC) => (
    <Form
      schema={schema}
      onSubmit={() => {}}
      options={{ defaultValues: { enableAggregation: true } }}
    >
      {() => <S />}
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
  (S: React.FC) => (
    <Form
      schema={schema}
      onSubmit={() => {}}
      options={{ defaultValues: { enableAggregation: false } }}
    >
      {() => <S />}
    </Form>
  ),
];

export const DefaultOpen: Story<AggregationProps> = args => (
  <AggregationSection {...args} />
);
DefaultOpen.args = {
  roleName: 'one',
  defaultOpen: true,
};
DefaultOpen.decorators = AggregationEnabled.decorators;
