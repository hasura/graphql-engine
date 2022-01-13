import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';
import { AllFKRelationships } from './useDataSourceFKRelationships.component';

export default {
  title: 'hooks/useDataSourceFKRelationships',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as ComponentMeta<typeof AllFKRelationships>;

export const Playground: ComponentStory<typeof AllFKRelationships> = args => {
  return <AllFKRelationships {...args} />;
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
