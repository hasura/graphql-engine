import type { ComponentStory } from '@storybook/react';

import * as React from 'react';
import { HeroEnabled as HeroEnabledComponent } from './HeroEnabled';
import { HeroDisabled as HeroDisabledComponent } from './HeroDisabled';

export default {
  title: 'Features/OpenTelemetryConfig/Hero',
};

// --------------------------------------------------
// STORY DEFINITION
// --------------------------------------------------
export const HeroEnabled: ComponentStory<typeof HeroEnabledComponent> = () => (
  <HeroEnabledComponent />
);
export const HeroDisabled: ComponentStory<typeof HeroDisabledComponent> =
  () => <HeroDisabledComponent />;
