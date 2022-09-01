import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta } from '@storybook/react';

import { QueryCollectionHeader } from './QueryCollectionHeader';
import { handlers } from '../../../QueryCollections/hooks/useQueryCollections/mocks/handlers.mock';
import { createMetadata } from '../../../QueryCollections/hooks/useQueryCollections/mocks/mockData';

export default {
  title: 'Features/Allow List/Query Collection Header',
  component: QueryCollectionHeader,
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers(1500, 'http://localhost:8080'),
  },
} as ComponentMeta<typeof QueryCollectionHeader>;

export const Primary = () => {
  const metadata = createMetadata();
  return (
    metadata.metadata.query_collections?.[0] && (
      <QueryCollectionHeader
        queryCollection={metadata.metadata.query_collections?.[0]}
      />
    )
  );
};
