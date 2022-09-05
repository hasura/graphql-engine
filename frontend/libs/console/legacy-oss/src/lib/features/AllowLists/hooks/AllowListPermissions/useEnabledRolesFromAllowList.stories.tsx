import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import ReactJson from 'react-json-view';
import { Meta, Story } from '@storybook/react';

import { useEnabledRolesFromAllowList } from './useEnabledRolesFromAllowList';
import { handlers } from './mock/handlers.mocks';

const UseEnabledRolesFromAllowList: React.FC = () => {
  const { data, isLoading, isError } =
    useEnabledRolesFromAllowList('rest-endpoint');

  if (isLoading) {
    return <div>Loading...</div>;
  }

  if (isError) {
    return <div>Error</div>;
  }

  return data ? <ReactJson src={data} /> : null;
};

export const Primary: Story = args => {
  return <UseEnabledRolesFromAllowList {...args} />;
};

Primary.args = {
  collectionName: 'rest-endpoint',
};

export default {
  title: 'hooks/Allow List Permission/useCreateNewRolePermission',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers(),
  },
} as Meta;
