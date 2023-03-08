import React from 'react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../../storybook/decorators/redux-decorator';
import { ComponentMeta } from '@storybook/react';

import { action } from '@storybook/addon-actions';
import { handlers } from '../../../../mocks/metadata.mock';

import { AllowListSidebar } from './AllowListSidebar';

export default {
  title: 'Features/Allow List/Allow List Sidebar',
  component: AllowListSidebar,
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers({ delay: 500 }),
  },
} as ComponentMeta<typeof AllowListSidebar>;

export const Primary = () => (
  <AllowListSidebar
    buildQueryCollectionHref={() => '#'}
    onQueryCollectionClick={action('onQueryCollectionClick')}
    onQueryCollectionCreate={action('onQueryCollectionCreate')}
    selectedCollectionQuery="allowed-queries"
  />
);
