import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../storybook/decorators/redux-decorator';
import { StoryObj, Meta } from '@storybook/react';
import React from 'react';
import { AllPrimaryKeys } from './useDataSourcePrimaryKeys.component';

export default {
  title: 'hooks/useDataSourcePrimaryKeys',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as Meta<typeof AllPrimaryKeys>;

export const Playground: StoryObj<typeof AllPrimaryKeys> = {
  render: args => {
    return <AllPrimaryKeys {...args} />;
  },

  parameters: {
    // Disable storybook for playground stories
    chromatic: { disableSnapshot: true },
  },

  argTypes: {
    driver: {
      options: ['postgres', 'bigquery', 'mssql', 'citus'],
      control: 'select',
    },
  },

  args: {
    driver: 'postgres',
    currentDatasource: 'default',
  },
};
