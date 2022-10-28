// Button.stories.ts|tsx

import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import { ModifyTable } from './ModifyTable';

const props = {
  table: ['Customer'],
  dataSourceName: 'sqlite-test',
  tableName: 'Customer',
};

export default {
  /* 👇 The title prop is optional.
   * See https://storybook.js.org/docs/react/configure/overview#configure-story-loading
   * to learn how to generate automatic titles
   */
  title: 'Data / ModifyTable',
  component: ModifyTable,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof ModifyTable>;

export const Primary: ComponentStory<typeof ModifyTable> = () => (
  <ModifyTable {...props} />
);
