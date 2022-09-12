import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { Root } from './HasuraFamiliaritySurvey';

export default {
  title: 'features/HasuraFamiliaritySurvey/Root',
  component: Root,
} as ComponentMeta<typeof Root>;

export const Base: Story = () => <Root onSkip={() => {}} />;
