import type { StoryFn } from '@storybook/react';

import * as React from 'react';
import { BadgeEnabled as BadgeEnabledComponent } from './BadgeEnabled';
import { BadgeDisabled as BadgeDisabledComponent } from './BadgeDisabled';
import { BadgeSkeleton as BadgeSkeletonComponent } from './BadgeSkeleton';

export default {
  title: 'Features/OpenTelemetry/Badge',
};

export const BadgeEnabled: StoryFn<typeof BadgeEnabledComponent> = () => (
  <BadgeEnabledComponent />
);
export const BadgeDisabled: StoryFn<typeof BadgeDisabledComponent> = () => (
  <BadgeDisabledComponent />
);
export const BadgeSkeleton: StoryFn<typeof BadgeSkeletonComponent> = () => (
  <BadgeSkeletonComponent />
);
