import React from 'react';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { ReactQueryDecorator } from '@hasura/shared/testing';
import { GDCTree } from './GDCTree';

export default {
  title: 'Data/Components/GDCTree',
  component: GDCTree,
  decorators: [ReactQueryDecorator()],
  argTypes: { onSelect: { action: 'clicked' } },
} as Meta<typeof GDCTree>;

export const Primary: StoryObj<typeof GDCTree> = {
  render: (args) => <GDCTree onSelect={args.onSelect} />,
};
