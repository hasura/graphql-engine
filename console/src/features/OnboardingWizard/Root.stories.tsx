import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { Root } from './Root';
import { handlers } from './mocks/handlers.mock';

export default {
  title: 'features/Onboarding Wizard/Root',
  component: Root,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as ComponentMeta<typeof Root>;

const mockGrowthClient = {
  getAllExperimentConfig: () => [
    {
      experiment: 'console_onboarding_wizard_v1',
      status: 'enabled',
      metadata: {},
      userActivity: {},
    },
  ],
  setAllExperimentConfig: () => Promise.resolve(),
};

export const Base: Story = () => (
  <Root growthExperimentsClient={mockGrowthClient} />
);
