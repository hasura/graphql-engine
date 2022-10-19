import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { DataGrid } from './DataGrid';
import { handlers } from '../../__mocks__/handlers.mock';

export default {
  title: 'Browse Rows/DataGrid 📒',
  component: DataGrid,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers('http://localhost:8080'),
  },
} as ComponentMeta<typeof DataGrid>;

export const Primary: ComponentStory<typeof DataGrid> = () => {
  return <DataGrid table={['Album']} dataSourceName="sqlite_test" />;
};
