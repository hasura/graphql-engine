import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { ConnectDBScreen } from './ConnectDBScreen';

export default {
  title: 'features/CloudOnboarding/Onboarding Wizard/Connect DB screen',
  component: ConnectDBScreen,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof ConnectDBScreen>;

export const Base: Story = () => (
  <ConnectDBScreen
    proceed={() => {}}
    dismissOnboarding={() => {}}
    dispatch={() => {}}
    setStepperIndex={() => {}}
  />
);

Base.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  // Expect element renders successfully, these texts are highly dynamic
  // according to product needs, and doesn't make sense to keep a lot of
  // "renders successfully" tests.
  expect(canvas.getByText('Connect Neon Database')).toBeVisible();
};
