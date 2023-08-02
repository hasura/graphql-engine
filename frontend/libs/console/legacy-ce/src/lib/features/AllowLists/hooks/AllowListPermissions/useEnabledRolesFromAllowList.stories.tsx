import React from 'react';
import { handlers } from '../../../../mocks/metadata.mock';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../../storybook/decorators/redux-decorator';
import ReactJson from 'react-json-view';
import { StoryObj, Meta } from '@storybook/react';

import { useEnabledRolesFromAllowList } from './useEnabledRolesFromAllowList';

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

export const Primary: StoryObj = {
  render: args => {
    return <UseEnabledRolesFromAllowList {...args} />;
  },

  args: {
    collectionName: 'rest-endpoint',
  },
};

export default {
  title: 'hooks/Allow List/useCreateNewRolePermission',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers({ delay: 500 }),
  },
} as Meta;
