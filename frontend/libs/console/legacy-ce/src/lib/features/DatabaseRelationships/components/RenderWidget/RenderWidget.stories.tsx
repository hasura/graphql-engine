// Button.stories.ts|tsx

import React from 'react';

import { StoryFn, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { RenderWidget } from './RenderWidget';
import { MODE } from '../../types';

export default {
  component: RenderWidget,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof RenderWidget>;

export const WithManualConfiguration: StoryFn<typeof RenderWidget> = () => (
  <RenderWidget
    dataSourceName="chinook"
    table={{ name: 'Album', schema: 'public' }}
    mode={MODE.CREATE}
    onCancel={() => {}}
    onSuccess={() => {}}
    onError={() => {}}
  />
);
