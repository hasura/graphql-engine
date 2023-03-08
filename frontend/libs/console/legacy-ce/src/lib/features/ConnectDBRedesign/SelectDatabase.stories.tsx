// Button.stories.ts|tsx

import React from 'react';
import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import { SelectDatabase } from '.';

export default {
  /* 👇 The title prop is optional.
   * See https://storybook.js.org/docs/react/configure/overview#configure-story-loading
   * to learn how to generate automatic titles
   */
  component: SelectDatabase,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof SelectDatabase>;

export const Primary: ComponentStory<typeof SelectDatabase> = () => (
  <div className="max-w-3xl">
    Note: This container has a max width set. When rendering this component keep
    width in mind to avoid it growing too large.
    <SelectDatabase />
  </div>
);
