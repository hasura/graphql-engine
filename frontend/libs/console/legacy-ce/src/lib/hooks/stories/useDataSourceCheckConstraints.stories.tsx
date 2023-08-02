import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../storybook/decorators/redux-decorator';
import { StoryObj, Meta } from '@storybook/react';
import React from 'react';
import { AllCheckConstraints } from './useDataSourceCheckConstraints.component';

export default {
  title: 'hooks/useDataSourceCheckConstraints',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as Meta<typeof AllCheckConstraints>;

export const Playground: StoryObj<typeof AllCheckConstraints> = {
  render: args => {
    return <AllCheckConstraints {...args} />;
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
