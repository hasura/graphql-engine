import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../storybook/decorators/redux-decorator';
import { StoryObj, Meta } from '@storybook/react';
import React from 'react';
import { AllUniqueKeys } from './useDataSourceUniqueKeys.component';

export default {
  title: 'hooks/useDataSourceUniqueKeys',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as Meta<typeof AllUniqueKeys>;

export const Playground: StoryObj<typeof AllUniqueKeys> = {
  render: args => {
    return <AllUniqueKeys {...args} />;
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
