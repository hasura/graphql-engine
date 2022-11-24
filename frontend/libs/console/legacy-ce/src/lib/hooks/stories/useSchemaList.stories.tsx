import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';
import { SchemaList } from './useSchemaList.component';

export default {
  title: 'hooks/useSchemaList',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as ComponentMeta<typeof SchemaList>;

export const Playground: ComponentStory<typeof SchemaList> = args => {
  return <SchemaList {...args} />;
};

Playground.args = {
  currentDatasource: 'default',
  driver: 'postgres',
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
