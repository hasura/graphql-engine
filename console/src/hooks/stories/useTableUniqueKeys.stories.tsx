import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';
import { UniqueKeys } from './useTableUniqueKeys.component';

export default {
  title: 'hooks/useTableUniqueKeys',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as ComponentMeta<typeof UniqueKeys>;

export const Playground: ComponentStory<typeof UniqueKeys> = args => {
  return <UniqueKeys {...args} />;
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
