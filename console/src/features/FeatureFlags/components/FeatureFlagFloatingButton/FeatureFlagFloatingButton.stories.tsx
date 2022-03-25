import { ComponentMeta } from '@storybook/react';
import React from 'react';
import { FeatureFlagFloatingButton } from './FeatureFlagFloatingButton';

export default {
  title: 'feature/FeatureFlags/FeatureFlagFloatingButton',
  component: FeatureFlagFloatingButton,
} as ComponentMeta<typeof FeatureFlagFloatingButton>;

export const Main = () => <FeatureFlagFloatingButton />;
