import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { ConnectDBScreen } from './ConnectDBScreen';

export default {
  title: 'features/Neon Integration/Connect DB screen',
  component: ConnectDBScreen,
} as ComponentMeta<typeof ConnectDBScreen>;

export const Base: Story = () => <ConnectDBScreen />;

Base.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  // Expect element renders successfully
  expect(
    await canvas.findByText('Create Neon Database for free')
  ).toBeVisible();
};
