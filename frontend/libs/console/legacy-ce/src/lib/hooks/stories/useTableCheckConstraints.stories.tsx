import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../storybook/decorators/redux-decorator';
import { StoryObj, Meta } from '@storybook/react';
import React from 'react';
import { CheckConstaints } from './useTableCheckConstraints.component';

export default {
  title: 'hooks/useTableCheckConstaints',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as Meta<typeof CheckConstaints>;

export const Playground: StoryObj<typeof CheckConstaints> = {
  render: args => {
    return <CheckConstaints {...args} />;
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
