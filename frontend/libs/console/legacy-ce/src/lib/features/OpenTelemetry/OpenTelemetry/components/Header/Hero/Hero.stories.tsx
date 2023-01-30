import type { ComponentStory } from '@storybook/react';

import * as React from 'react';
import { HeroEnabled as HeroEnabledComponent } from './HeroEnabled';
import { HeroDisabled as HeroDisabledComponent } from './HeroDisabled';
import { HeroSkeleton as HeroSkeletonComponent } from './HeroSkeleton';

export default {
  title: 'Features/OpenTelemetry/Hero',
};

// --------------------------------------------------
// STORY DEFINITION
// --------------------------------------------------
export const HeroEnabled: ComponentStory<typeof HeroEnabledComponent> = () => (
  <HeroEnabledComponent />
);
export const HeroDisabled: ComponentStory<
  typeof HeroDisabledComponent
> = () => <HeroDisabledComponent />;
export const HeroSkeleton: ComponentStory<
  typeof HeroSkeletonComponent
> = () => <HeroSkeletonComponent />;
