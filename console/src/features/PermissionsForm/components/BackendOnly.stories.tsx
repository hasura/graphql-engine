import React from 'react';
import { Meta, Story } from '@storybook/react';
import { z } from 'zod';
import { SimpleForm } from '@/new-components/Form';

import { BackendOnlySection, BackEndOnlySectionProps } from './BackendOnly';

export default {
  title:
    'Features/Permissions Tab/Permissions Form/Components/Backend Only Section',
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
    <SimpleForm
      schema={schema}
      onSubmit={() => {}}
      options={{ defaultValues: { backendOnly: true } }}
      className="p-4"
    >
      <StoryComponent />
    </SimpleForm>
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
    <SimpleForm
      schema={schema}
      onSubmit={() => {}}
      options={{ defaultValues: { backendOnly: false } }}
      className="p-4"
    >
      <StoryComponent />
    </SimpleForm>
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
