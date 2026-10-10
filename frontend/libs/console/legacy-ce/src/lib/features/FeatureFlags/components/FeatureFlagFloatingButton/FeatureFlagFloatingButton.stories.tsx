import { Meta } from '@storybook/react-webpack5';
import React from 'react';
import { FeatureFlagFloatingButton } from './FeatureFlagFloatingButton';

export default {
  title: 'features/FeatureFlags/FeatureFlagFloatingButton',
  component: FeatureFlagFloatingButton,
} as Meta<typeof FeatureFlagFloatingButton>;

export const Main = () => <FeatureFlagFloatingButton />;
