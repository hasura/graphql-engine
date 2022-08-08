import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';

import { ComponentStory, ComponentMeta } from '@storybook/react';
import { Connect } from './Connect';

export default {
  title: 'Data/Connect',
  component: Connect,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof Connect>;

export const Primary: ComponentStory<typeof Connect> = () => <Connect />;
