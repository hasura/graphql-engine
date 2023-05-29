import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../storybook/decorators/redux-decorator';
import { StoryObj, Meta } from '@storybook/react';
import React from 'react';
import { NonTrackableFunctions } from './useNonTrackableFunctions.component';

export default {
  title: 'hooks/useNonTrackableFunctions',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as Meta<typeof NonTrackableFunctions>;

export const Playground: StoryObj<typeof NonTrackableFunctions> = {
  render: args => {
    return <NonTrackableFunctions {...args} />;
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
