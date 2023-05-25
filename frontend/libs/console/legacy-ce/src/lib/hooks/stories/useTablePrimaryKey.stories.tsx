import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../storybook/decorators/redux-decorator';
import { StoryObj, Meta } from '@storybook/react';
import React from 'react';
import { PrimaryKeys } from './useTablePrimaryKey.component';

export default {
  title: 'hooks/useTablePrimaryKey',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as Meta<typeof PrimaryKeys>;

export const Playground: StoryObj<typeof PrimaryKeys> = {
  render: args => {
    return <PrimaryKeys {...args} />;
  },

  args: {
    currentDatasource: 'default',
    driver: 'postgres',
    table: {
      name: 'person',
      schema: 'public',
    },
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
};
