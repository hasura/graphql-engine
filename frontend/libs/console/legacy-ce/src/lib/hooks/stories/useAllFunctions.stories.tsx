import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../storybook/decorators/redux-decorator';
import { StoryObj, Meta } from '@storybook/react';
import React from 'react';
import { AllFunctions } from './useAllFunctions.component';

export default {
  title: 'hooks/useAllFunctions',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as Meta<typeof AllFunctions>;

export const Playground: StoryObj<typeof AllFunctions> = {
  render: args => {
    return <AllFunctions {...args} />;
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
