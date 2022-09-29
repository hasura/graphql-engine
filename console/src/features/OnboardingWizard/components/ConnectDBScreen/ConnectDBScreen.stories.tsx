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
  />
);

WithoutNeon.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  // Expect element renders successfully
  expect(
    await canvas.findByText('Welcome to your new Hasura project!')
  ).toBeVisible();
  expect(
    await canvas.findByText(
      "Let's get started by connecting your first database"
    )
  ).toBeVisible();
};

export const WithNeon: Story = () => (
  <ConnectDBScreen
    proceed={() => {}}
    dismissOnboarding={() => {}}
    hasNeonAccess
    dispatch={() => {}}
  />
);

WithNeon.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  // Expect element renders successfully
  expect(
    await canvas.findByText('Welcome to your new Hasura project!')
  ).toBeVisible();
  expect(
    await canvas.findByText(
      "Let's get started by connecting your first database"
    )
  ).toBeVisible();
  expect(await canvas.findByText('Connect Neon Database')).toBeVisible();

  expect(await canvas.findByText('Need a new database?')).toBeVisible();

  expect(
    await canvas.findByText(
      'Hasura has partnered with Neon to help you seamlessly create your database with their serverless Postgres platform.'
    )
  ).toBeVisible();
};
