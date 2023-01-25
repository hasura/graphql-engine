import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';
import { TrackableFunctions } from './useTrackableFunctions.component';

export default {
  title: 'hooks/useTrackableFunctions',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as ComponentMeta<typeof TrackableFunctions>;

export const Playground: ComponentStory<typeof TrackableFunctions> = args => {
  return <TrackableFunctions {...args} />;
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
