import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';
import { AllTables } from './useDataSourceTables.component';

export default {
  title: 'hooks/useDataSourceTables',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as ComponentMeta<typeof AllTables>;

export const Playground: ComponentStory<typeof AllTables> = args => {
  return <AllTables {...args} />;
};

Playground.parameters = {
  // Disable storybook for playground stories
  chromatic: { disableSnapshot: true },
};

Playground.argTypes = {
  driver: {
    options: ['postgres', 'bigquery', 'mssql', 'citus'],
    control: 'select',
  },
};

Playground.args = {
  driver: 'postgres',
  currentDatasource: 'default',
};
