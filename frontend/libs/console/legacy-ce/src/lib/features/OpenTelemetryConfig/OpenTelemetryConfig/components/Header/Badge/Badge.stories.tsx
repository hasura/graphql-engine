import type { ComponentStory } from '@storybook/react';

import * as React from 'react';
import { BadgeEnabled as BadgeEnabledComponent } from './BadgeEnabled';
import { BadgeDisabled as BadgeDisabledComponent } from './BadgeDisabled';
import { BadgeSkeleton as BadgeSkeletonComponent } from './BadgeSkeleton';

export default {
  title: 'Features/OpenTelemetryConfig/Badge',
};

// --------------------------------------------------
// STORY DEFINITION
// --------------------------------------------------
export const BadgeEnabled: ComponentStory<
  typeof BadgeEnabledComponent
> = () => <BadgeEnabledComponent />;
export const BadgeDisabled: ComponentStory<
  typeof BadgeDisabledComponent
> = () => <BadgeDisabledComponent />;
export const BadgeSkeleton: ComponentStory<
  typeof BadgeSkeletonComponent
> = () => <BadgeSkeletonComponent />;
