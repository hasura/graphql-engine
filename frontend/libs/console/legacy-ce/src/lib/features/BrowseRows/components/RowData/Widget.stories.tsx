import React from 'react';

import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';

import { Widget } from './Widget';

export default {
  component: Widget,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof Widget>;

export const Primary: ComponentStory<typeof Widget> = () => (
  <Widget
    table={{ name: 'customers', schema: 'sales' }}
    dataSourceName="bikes"
  />
);
