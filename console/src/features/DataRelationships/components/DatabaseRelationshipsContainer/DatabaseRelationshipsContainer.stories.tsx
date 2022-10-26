import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ComponentMeta } from '@storybook/react';

import { DatabaseRelationshipsContainer } from './DatabaseRelationshipsContainer';
import { handlers } from '../ManualLocalRelationshipWidget/__mocks__/localrelationships.mock';

export default {
  title: 'Relationships/Database Relationships Container',
  component: DatabaseRelationshipsContainer,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as ComponentMeta<typeof DatabaseRelationshipsContainer>;

export const Primary = () => (
  <DatabaseRelationshipsContainer
    dataSourceName="sqlite_test"
    table={['Album']}
  />
);
