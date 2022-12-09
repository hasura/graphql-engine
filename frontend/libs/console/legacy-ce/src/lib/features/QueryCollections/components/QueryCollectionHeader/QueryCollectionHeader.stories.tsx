import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta } from '@storybook/react';
import { handlers, createDefaultInitialData } from '@/mocks/metadata.mock';
import { action } from '@storybook/addon-actions';

import { QueryCollectionHeader } from './QueryCollectionHeader';

export default {
  title: 'Features/Query Collections/Query Collection Header',
  component: QueryCollectionHeader,
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers({ delay: 500 }),
  },
} as ComponentMeta<typeof QueryCollectionHeader>;

export const Primary = () => {
  const metadata = createDefaultInitialData();
  return (
    metadata.metadata.query_collections?.[0] && (
      <QueryCollectionHeader
        onDelete={action('delete')}
        onRename={action('rename')}
        queryCollection={metadata.metadata.query_collections?.[0]}
      />
    )
  );
};
