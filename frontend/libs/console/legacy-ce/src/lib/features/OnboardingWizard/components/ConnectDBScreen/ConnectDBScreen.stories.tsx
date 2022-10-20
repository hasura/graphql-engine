import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { ConnectDBScreen } from './ConnectDBScreen';

export default {
  title: 'features/Onboarding Wizard/Connect DB screen',
  component: ConnectDBScreen,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof ConnectDBScreen>;

export const WithoutNeon: Story = () => (
  <ConnectDBScreen
    proceed={() => {}}
    dismissOnboarding={() => {}}
    dispatch={() => {}}
    hasNeonAccess={!true}
    setStepperIndex={() => {}}
  />
);

WithoutNeon.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  // Expect element renders successfully
  expect(canvas.getByText('Connect Your Database')).toBeVisible();
};

export const WithNeon: Story = () => (
  <ConnectDBScreen
    proceed={() => {}}
    dismissOnboarding={() => {}}
    hasNeonAccess
    dispatch={() => {}}
    setStepperIndex={() => {}}
  />
);

WithNeon.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  // Expect element renders successfully, these texts are highly dynamic
  // according to product needs, and doesn't make sense to keep a lot of
  // "renders successfully" tests.
  expect(canvas.getByText('Connect Neon Database')).toBeVisible();
};
