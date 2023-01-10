import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';
import { PrimaryKeys } from './useTablePrimaryKey.component';

export default {
  title: 'hooks/useTablePrimaryKey',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as ComponentMeta<typeof PrimaryKeys>;

export const Playground: ComponentStory<typeof PrimaryKeys> = args => {
  return <PrimaryKeys {...args} />;
};

Playground.args = {
  currentDatasource: 'default',
  driver: 'postgres',
  table: {
    name: 'person',
    schema: 'public',
  },
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
