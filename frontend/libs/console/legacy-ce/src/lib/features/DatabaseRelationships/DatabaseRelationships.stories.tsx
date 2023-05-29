import React from 'react';

import { StoryFn, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { DatabaseRelationships } from './DatabaseRelationships';
import { handlers } from './handler.mock';

export default {
  component: DatabaseRelationships,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta<typeof DatabaseRelationships>;

export const Basic: StoryFn<typeof DatabaseRelationships> = () => (
  <DatabaseRelationships
    dataSourceName="bikes"
    table={{ name: 'products', schema: 'production' }}
  />
);
