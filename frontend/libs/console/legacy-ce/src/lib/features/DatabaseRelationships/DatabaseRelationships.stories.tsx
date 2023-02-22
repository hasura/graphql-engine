import React from 'react';

import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { DatabaseRelationships } from './DatabaseRelationships';
import { handlers } from './handler.mock';

export default {
  component: DatabaseRelationships,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as ComponentMeta<typeof DatabaseRelationships>;

export const Basic: ComponentStory<typeof DatabaseRelationships> = () => (
  <DatabaseRelationships
    dataSourceName="bikes"
    table={{ name: 'products', schema: 'production' }}
  />
);
