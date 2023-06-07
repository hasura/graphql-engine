import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../storybook/decorators/redux-decorator';
import { StoryObj, Meta } from '@storybook/react';
import React from 'react';
import { SingleTable } from './useSingleTable.component';

export default {
  title: 'hooks/useSingleTable',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as Meta<typeof SingleTable>;

export const Playground: StoryObj<typeof SingleTable> = {
  render: args => {
    return <SingleTable {...args} />;
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
