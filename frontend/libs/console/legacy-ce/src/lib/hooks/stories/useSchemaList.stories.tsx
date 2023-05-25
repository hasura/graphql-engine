import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../storybook/decorators/redux-decorator';
import { StoryObj, Meta } from '@storybook/react';
import React from 'react';
import { SchemaList } from './useSchemaList.component';

export default {
  title: 'hooks/useSchemaList',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as Meta<typeof SchemaList>;

export const Playground: StoryObj<typeof SchemaList> = {
  render: args => {
    return <SchemaList {...args} />;
  },

  args: {
    currentDatasource: 'default',
    driver: 'postgres',
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
