import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta } from '@storybook/react';

import { AllowListSidebar } from './AllowListSidebar';
import { handlers } from '../../../QueryCollections/hooks/useQueryCollections/mocks/handlers.mock';

export default {
  title: 'Features/Allow List/Allow List Sidebar',
  component: AllowListSidebar,
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers(1500),
  },
} as ComponentMeta<typeof AllowListSidebar>;

export const Primary = () => (
  <AllowListSidebar selectedCollectionQuery="allowed_queries" />
);
