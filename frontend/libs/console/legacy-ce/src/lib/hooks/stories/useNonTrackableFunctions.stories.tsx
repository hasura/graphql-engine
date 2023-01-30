import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';
import { NonTrackableFunctions } from './useNonTrackableFunctions.component';

export default {
  title: 'hooks/useNonTrackableFunctions',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as ComponentMeta<typeof NonTrackableFunctions>;

export const Playground: ComponentStory<
  typeof NonTrackableFunctions
> = args => {
  return <NonTrackableFunctions {...args} />;
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
