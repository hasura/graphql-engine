import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../storybook/decorators/redux-decorator';
import { StoryObj, Meta } from '@storybook/react';
import React from 'react';
import { TrackableFunctions } from './useTrackableFunctions.component';

export default {
  title: 'hooks/useTrackableFunctions',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as Meta<typeof TrackableFunctions>;

export const Playground: StoryObj<typeof TrackableFunctions> = {
  render: args => {
    return <TrackableFunctions {...args} />;
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
