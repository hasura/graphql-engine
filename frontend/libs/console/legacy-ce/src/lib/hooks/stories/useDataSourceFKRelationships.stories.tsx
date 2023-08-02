import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../storybook/decorators/redux-decorator';
import { StoryObj, Meta } from '@storybook/react';
import React from 'react';
import { AllFKRelationships } from './useDataSourceFKRelationships.component';

export default {
  title: 'hooks/useDataSourceFKRelationships',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as Meta<typeof AllFKRelationships>;

export const Playground: StoryObj<typeof AllFKRelationships> = {
  render: args => {
    return <AllFKRelationships {...args} />;
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
