import React from 'react';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { expect, within } from 'storybook/test';
import { NeonBanner } from './NeonBanner';

export default {
  title: 'features/CloudOnboarding/Onboarding Wizard/NeonConnectBanner',
  component: NeonBanner,
} as Meta<typeof NeonBanner>;

export const Creating: StoryObj = {
  render: () => (
    <NeonBanner
      onClickConnect={() => window.alert('clicked connect button')}
      status={{ status: 'loading' }}
      buttonText="Creating Neon Database"
      setStepperIndex={() => {}}
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Expect element renders successfully
    await expect(canvas.getByText('Creating Neon Database')).toBeVisible();
    // Expect button disabled state to be as expected
    await expect(
      canvas.getByTestId('onboarding-wizard-neon-connect-db-button'),
    ).toBeVisible();
    await expect(
      canvas.getByTestId('onboarding-wizard-neon-connect-db-button'),
    ).toBeDisabled();
  },
};

export const Error: StoryObj = {
  render: () => (
    <NeonBanner
      onClickConnect={() => window.alert('clicked connect button')}
      status={{
        status: 'error',
        errorTitle: 'Your Neon Database connection failed',
        errorDescription: 'You have exceeded the free project limit on Neon.',
      }}
      buttonText="Try Again"
      icon="refresh"
      setStepperIndex={() => {}}
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Expect element renders successfully
    await expect(canvas.getByText('Try Again')).toBeVisible();
    await expect(canvas.getByText('Try Again')).not.toBeDisabled();

    // Expect element rend
    await expect(
      canvas.getByText('Your Neon Database connection failed'),
    ).toBeVisible();
    await expect(
      canvas.getByText('You have exceeded the free project limit on Neon.'),
    ).toBeVisible();

    await expect(
      canvas.getByTestId('onboarding-wizard-neon-connect-db-button'),
    ).toBeVisible();
    await expect(
      canvas.getByTestId('onboarding-wizard-neon-connect-db-button'),
    ).not.toBeDisabled();
  },
};
