import React from 'react';
import { Story, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
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
