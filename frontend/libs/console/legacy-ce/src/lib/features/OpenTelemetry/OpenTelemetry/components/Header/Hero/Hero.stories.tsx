import type { StoryFn } from '@storybook/react';

import * as React from 'react';
import { HeroEnabled as HeroEnabledComponent } from './HeroEnabled';
import { HeroDisabled as HeroDisabledComponent } from './HeroDisabled';
import { HeroSkeleton as HeroSkeletonComponent } from './HeroSkeleton';

export default {
  title: 'Features/OpenTelemetry/Hero',
};

export const HeroEnabled: StoryFn<typeof HeroEnabledComponent> = () => (
  <HeroEnabledComponent />
);
export const HeroDisabled: StoryFn<typeof HeroDisabledComponent> = () => (
  <HeroDisabledComponent />
);
export const HeroSkeleton: StoryFn<typeof HeroSkeletonComponent> = () => (
  <HeroSkeletonComponent />
);
