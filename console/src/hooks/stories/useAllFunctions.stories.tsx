import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';
import { AllFunctions } from './useAllFunctions.component';

export default {
  title: 'hooks/useAllFunctions',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as ComponentMeta<typeof AllFunctions>;

export const Playground: ComponentStory<typeof AllFunctions> = args => {
  return <AllFunctions {...args} />;
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
