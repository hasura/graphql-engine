import React from 'react';
import { Story, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { within, userEvent } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { DataTarget } from '@/features/Datasources';
import { handlers } from '../../hooks/mocks/handlers.mock';

import {
  SuggestedRelationships,
  SuggestedRelationshipProps,
} from './SuggestedRelationships';

export default {
  title: 'Features/Data Relationships/Suggested Relationships',
  component: SuggestedRelationships,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta;

const target: DataTarget = {
  database: 'default',
  schema: 'public',
  table: 'user',
};

export const Primary: Story<SuggestedRelationshipProps> = args => (
  <SuggestedRelationships {...args} />
);
Primary.args = {
  target,
};

export const WithInteraction: Story<SuggestedRelationshipProps> = args => (
  <SuggestedRelationships {...args} />
);
WithInteraction.args = Primary.args;
WithInteraction.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  const button = await canvas.findByRole('button', { name: 'Add' });
  userEvent.click(button);

  const label = await canvas.findByLabelText('Relationship Name');
  userEvent.type(label, 's');
  expect(label).toBeInTheDocument();

  const submitButton = await canvas.findByRole('button', {
    name: 'Add Relationship',
  });
  userEvent.click(submitButton);
};
