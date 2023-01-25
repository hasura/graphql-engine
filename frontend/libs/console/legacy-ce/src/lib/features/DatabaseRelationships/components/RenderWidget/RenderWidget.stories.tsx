// Button.stories.ts|tsx

import React from 'react';

import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { RenderWidget } from './RenderWidget';
import { MODE } from '../../types';

export default {
  /* 👇 The title prop is optional.
   * See https://storybook.js.org/docs/react/configure/overview#configure-story-loading
   * to learn how to generate automatic titles
   */
  title: 'GDC Console/Relationships/Create Relationship',
  component: RenderWidget,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof RenderWidget>;

export const WithManualConfiguration: ComponentStory<
  typeof RenderWidget
> = () => (
  <RenderWidget
    dataSourceName="bikes"
    table={{ name: 'orders', schema: 'sales' }}
    mode={MODE.CREATE}
    onCancel={() => {}}
    onSuccess={() => {}}
    onError={() => {}}
  />
);
