import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { NeonBanner } from './NeonBanner';

export default {
  title: 'features/CloudOnboarding/Onboarding Wizard/NeonConnectBanner',
  component: NeonBanner,
} as Meta<typeof NeonBanner>;

export const Base: StoryObj = {
  render: () => (
    <NeonBanner
      onClickConnect={() => window.alert('clicked connect button')}
      status={{ status: 'default' }}
      buttonText="Create a Neon Database"
      setStepperIndex={() => {}}
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Expect element renders successfully
    expect(canvas.getByText('Create a Neon Database')).toBeVisible();
    expect(
      canvas.getByTestId('onboarding-wizard-neon-connect-db-button')
    ).toBeVisible();
    expect(
      canvas.getByTestId('onboarding-wizard-neon-connect-db-button')
    ).not.toBeDisabled();
  },
};

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
    expect(canvas.getByText('Creating Neon Database')).toBeVisible();
    // Expect button disabled state to be as expected
    expect(
      canvas.getByTestId('onboarding-wizard-neon-connect-db-button')
    ).toBeVisible();
    expect(
      canvas.getByTestId('onboarding-wizard-neon-connect-db-button')
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
    expect(canvas.getByText('Try Again')).toBeVisible();
    expect(canvas.getByText('Try Again')).not.toBeDisabled();

    // Expect element rend
    expect(
      canvas.getByText('Your Neon Database connection failed')
    ).toBeVisible();
    expect(
      canvas.getByText('You have exceeded the free project limit on Neon.')
    ).toBeVisible();

    expect(
      canvas.getByTestId('onboarding-wizard-neon-connect-db-button')
    ).toBeVisible();
    expect(
      canvas.getByTestId('onboarding-wizard-neon-connect-db-button')
    ).not.toBeDisabled();
  },
};
