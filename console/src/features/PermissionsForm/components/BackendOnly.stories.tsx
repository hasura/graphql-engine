import React from 'react';
import { Story, Meta } from '@storybook/react';
import { z } from 'zod';
import { Form } from '@/new-components/Form';

import { BackendOnlySection, BackEndOnlySectionProps } from './BackendOnly';

export default {
  title: 'Permissions Form/Components/Backend Only Section',
  component: BackendOnlySection,
  parameters: {
    // Disable storybook for playground stories
    chromatic: { disableSnapshot: true },
  },
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
  (StoryComponent: React.FC) => (
    <Form
      schema={schema}
      onSubmit={() => {}}
      options={{ defaultValues: { backendOnly: true } }}
    >
      {() => <StoryComponent />}
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
  (StoryComponent: React.FC) => (
    <Form
      schema={schema}
      onSubmit={() => {}}
      options={{ defaultValues: { backendOnly: false } }}
    >
      {() => <StoryComponent />}
    </Form>
  ),
];

export const Showcase: Story<BackEndOnlySectionProps> = args => (
  <BackendOnlySection {...args} />
);
Showcase.args = {
  queryType: 'insert',
  defaultOpen: true,
};
Showcase.parameters = {
  // Enable storybook for Showcase stories
  chromatic: { disableSnapshot: false },
};
Showcase.decorators = BackendOnlyEnabled.decorators;
