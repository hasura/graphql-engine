import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { ConnectDBScreen } from './ConnectDBScreen';

export default {
  title: 'features/Onboarding Wizard/Connect DB screen',
  component: ConnectDBScreen,
} as ComponentMeta<typeof ConnectDBScreen>;

export const Base: Story = () => <ConnectDBScreen closeWizard={() => {}} />;

Base.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  // Expect element renders successfully
  expect(
    await canvas.findByText('Welcome to your new Hasura project')
  ).toBeVisible();
  expect(
    await canvas.findByText(
      "Let's get started by connecting your first database"
    )
  ).toBeVisible();
};
