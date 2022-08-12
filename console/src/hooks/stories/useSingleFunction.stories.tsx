import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';
import { SingleFunction } from './useSingleFunction.component';

export default {
  title: 'hooks/useSingleFunction',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as ComponentMeta<typeof SingleFunction>;

export const Playground: ComponentStory<typeof SingleFunction> = args => {
  return <SingleFunction {...args} />;
};

Playground.args = {
  currentDatasource: 'default',
  driver: 'postgres',
  myFunction: {
    name: 'search_houses',
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
