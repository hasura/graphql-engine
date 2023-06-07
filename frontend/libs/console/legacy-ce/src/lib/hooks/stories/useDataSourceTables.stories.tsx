import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../storybook/decorators/redux-decorator';
import { StoryObj, Meta } from '@storybook/react';
import React from 'react';
import { AllTables } from './useDataSourceTables.component';

export default {
  title: 'hooks/useDataSourceTables',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as Meta<typeof AllTables>;

export const Playground: StoryObj<typeof AllTables> = {
  render: args => {
    return <AllTables {...args} />;
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
