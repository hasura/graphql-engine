import React from 'react';

import { StoryFn, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';

import { Widget } from './Widget';

export default {
  component: Widget,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof Widget>;

export const Primary: StoryFn<typeof Widget> = () => (
  <Widget
    table={{ name: 'customers', schema: 'sales' }}
    dataSourceName="bikes"
  />
);
