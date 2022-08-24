import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { Root } from './Root';

export default {
  title: 'features/Onboarding Wizard/Root',
  component: Root,
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
