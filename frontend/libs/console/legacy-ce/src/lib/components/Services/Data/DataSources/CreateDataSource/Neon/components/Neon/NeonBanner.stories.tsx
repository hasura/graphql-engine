import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { NeonBanner } from './NeonBanner';

export default {
  title: 'features/Neon Integration/NeonBanner',
  component: NeonBanner,
} as Meta<typeof NeonBanner>;

export const Base: StoryObj = {
  render: () => (
    <NeonBanner
      onClickConnect={() => window.alert('clicked connect button')}
      status={{
        status: 'default',
      }}
      buttonText="Create Neon Database for free"
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Expect element renders successfully
    expect(
      await canvas.findByText('Create Neon Database for free')
    ).toBeVisible();
    expect(await canvas.getByTestId('neon-connect-db-button')).toBeVisible();
    expect(
      await canvas.getByTestId('neon-connect-db-button')
    ).not.toBeDisabled();
  },
};

export const Loading: StoryObj = {
  render: () => (
    <NeonBanner
      status={{
        status: 'loading',
      }}
      buttonText="Authenticating"
      onClickConnect={() => null}
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Expect element renders successfully
    expect(await canvas.findByText('Authenticating')).toBeVisible();
    // Expect button disabled state to be as expected
    expect(await canvas.getByTestId('neon-connect-db-button')).toBeVisible();
    expect(await canvas.getByTestId('neon-connect-db-button')).toBeDisabled();
  },
};

export const Creating: StoryObj = {
  render: () => (
    <NeonBanner
      status={{
        status: 'loading',
      }}
      buttonText="Creating Database"
      onClickConnect={() => null}
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Expect element renders successfully
    expect(await canvas.findByText('Creating Database')).toBeVisible();
    // Expect element renders successfully
    expect(await canvas.getByTestId('neon-connect-db-button')).toBeVisible();
    expect(await canvas.getByTestId('neon-connect-db-button')).toBeDisabled();
  },
};

export const Error: StoryObj = {
  render: () => (
    <NeonBanner
      onClickConnect={() => window.alert('clicked connect button')}
      status={{
        status: 'error',
        errorTitle: 'Error creating database',
        errorDescription: 'You have exceeded the free project limit on Neon.',
      }}
      buttonText="Try Again"
      icon="refresh"
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Expect element renders successfully
    expect(await canvas.findByText('Try Again')).toBeVisible();
    expect(await canvas.findByText('Try Again')).not.toBeDisabled();

    // Expect element rend
    expect(await canvas.findByText('Error creating database')).toBeVisible();
    expect(
      await canvas.findByText(
        'You have exceeded the free project limit on Neon.'
      )
    ).toBeVisible();

    expect(await canvas.getByTestId('neon-connect-db-button')).toBeVisible();
    expect(
      await canvas.getByTestId('neon-connect-db-button')
    ).not.toBeDisabled();
  },
};
