import React from 'react';

import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';

import { TrackTables } from '../TrackTables';
import { handlers } from './handlers.mock';

export default {
  title: 'Data/Components/TrackTables',
  component: TrackTables,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as ComponentMeta<typeof TrackTables>;

export const Primary: ComponentStory<typeof TrackTables> = () => (
  <TrackTables dataSourceName="chinook" />
);
