import React from 'react';

import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';

import { GDCTree } from './GDCTree';
import { GDCSource } from './types';

export default {
  title: 'Data/Components/GDCTree',
  component: GDCTree,
  decorators: [ReactQueryDecorator()],
  argTypes: { onSelect: { action: 'clicked' } },
} as ComponentMeta<typeof GDCTree>;

export const Primary: ComponentStory<typeof GDCTree> = args => (
  <GDCTree<GDCSource> onSelect={args.onSelect} />
);
