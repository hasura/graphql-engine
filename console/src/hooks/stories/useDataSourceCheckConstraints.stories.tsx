import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';
import { AllCheckConstraints } from './useDataSourceCheckConstraints.component';

export default {
  title: 'hooks/useDataSourceCheckConstraints',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as ComponentMeta<typeof AllCheckConstraints>;

export const Playground: ComponentStory<typeof AllCheckConstraints> = args => {
  return <AllCheckConstraints {...args} />;
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
