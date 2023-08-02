import { Meta } from '@storybook/react';
import React from 'react';
import { FeatureFlagFloatingButton } from './FeatureFlagFloatingButton';

export default {
  title: 'features/FeatureFlags/FeatureFlagFloatingButton',
  component: FeatureFlagFloatingButton,
} as Meta<typeof FeatureFlagFloatingButton>;

export const Main = () => <FeatureFlagFloatingButton />;
