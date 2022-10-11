import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { DataGrid } from './DataGrid';

export default {
  title: 'Browse Rows/DataGrid',
  component: DataGrid,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof DataGrid>;

export const Primary: ComponentStory<typeof DataGrid> = () => {
  return <DataGrid table={['Track']} dataSourceName="sqlite_test" />;
};
