// Button.stories.ts|tsx

import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import { ManageTable } from './ManageTable';

const props = {
  table: ['Customer'],
  dataSourceName: 'sqlite-test',
};

export default {
  /* 👇 The title prop is optional.
   * See https://storybook.js.org/docs/react/configure/overview#configure-story-loading
   * to learn how to generate automatic titles
   */
  title: 'Data / ManageTable',
  component: ManageTable,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof ManageTable>;

export const Primary: ComponentStory<typeof ManageTable> = () => (
  <ManageTable {...props} />
);
