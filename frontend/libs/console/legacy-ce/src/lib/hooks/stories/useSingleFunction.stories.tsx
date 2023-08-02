import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../storybook/decorators/redux-decorator';
import { StoryObj, Meta } from '@storybook/react';
import React from 'react';
import { SingleFunction } from './useSingleFunction.component';

export default {
  title: 'hooks/useSingleFunction',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as Meta<typeof SingleFunction>;

export const Playground: StoryObj<typeof SingleFunction> = {
  render: args => {
    return <SingleFunction {...args} />;
  },

  args: {
    currentDatasource: 'default',
    driver: 'postgres',
    myFunction: {
      name: 'search_houses',
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
