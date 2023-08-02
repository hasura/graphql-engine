import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { z } from 'zod';
import { SimpleForm } from '../../../../new-components/Form';

import { BackendOnlySection, BackEndOnlySectionProps } from './BackendOnly';

export default {
  component: BackendOnlySection,
  parameters: {
    // Disable storybook for playground stories
    chromatic: { disableSnapshot: true },
  },
} as Meta;

const schema = z.object({
  backendOnly: z.boolean(),
});

export const BackendOnlyEnabled: StoryObj<BackEndOnlySectionProps> = {
  args: {
    queryType: 'insert',
  },

  decorators: [
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
  ],
};

export const BackendOnlyDisabled: StoryObj<BackEndOnlySectionProps> = {
  args: {
    queryType: 'insert',
  },

  decorators: [
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
  ],
};

export const Showcase: StoryObj<BackEndOnlySectionProps> = {
  args: {
    queryType: 'insert',
    defaultOpen: true,
  },

  parameters: {
    // Enable storybook for Showcase stories
    chromatic: { disableSnapshot: false },
  },

  decorators: BackendOnlyEnabled.decorators,
};
