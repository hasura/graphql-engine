import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';
import { CheckConstaints } from './useTableCheckConstraints.component';

export default {
  title: 'hooks/useTableCheckConstaints',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as ComponentMeta<typeof CheckConstaints>;

export const Playground: ComponentStory<typeof CheckConstaints> = args => {
  return <CheckConstaints {...args} />;
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
