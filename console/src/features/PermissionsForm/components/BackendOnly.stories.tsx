import React from 'react';
import { Story, Meta } from '@storybook/react';
import { z } from 'zod';
import { Form } from '@/new-components/Form';

import { BackendOnlySection, BackEndOnlySectionProps } from './BackendOnly';

export default {
  title: 'Permissions Form/Components/Backend Only Section',
  component: BackendOnlySection,
} as Meta;

const schema = z.object({
  backendOnly: z.boolean(),
});

export const BackendOnlyEnabled: Story<BackEndOnlySectionProps> = args => (
  <BackendOnlySection {...args} />
);
BackendOnlyEnabled.args = {
  queryType: 'insert',
};
BackendOnlyEnabled.decorators = [
  (S: React.FC) => (
    <Form
      schema={schema}
      onSubmit={() => {}}
      options={{ defaultValues: { backendOnly: true } }}
    >
      {() => <S />}
    </Form>
  ),
];

export const BackendOnlyDisabled: Story<BackEndOnlySectionProps> = args => (
  <BackendOnlySection {...args} />
);
BackendOnlyDisabled.args = {
  queryType: 'insert',
};
BackendOnlyDisabled.decorators = [
  (S: React.FC) => (
    <Form
      schema={schema}
      onSubmit={() => {}}
      options={{ defaultValues: { backendOnly: false } }}
    >
      {() => <S />}
    </Form>
  ),
];

export const DefaultOpen: Story<BackEndOnlySectionProps> = args => (
  <BackendOnlySection {...args} />
);
DefaultOpen.args = {
  queryType: 'insert',
  defaultOpen: true,
};
DefaultOpen.decorators = BackendOnlyEnabled.decorators;
