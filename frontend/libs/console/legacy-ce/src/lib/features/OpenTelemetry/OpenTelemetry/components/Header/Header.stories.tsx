import type { ComponentStory } from '@storybook/react';
import * as React from 'react';

import { Header } from './Header';

export default {
  title: 'Features/OpenTelemetry/Header',
};

// --------------------------------------------------
// STORY DEFINITION
// --------------------------------------------------
export const HeaderEnabled: ComponentStory<typeof Header> = () => (
  <Header mode="enabled" />
);
export const HeaderDisabled: ComponentStory<typeof Header> = () => (
  <Header mode="disabled" />
);
export const HeaderSkeleton: ComponentStory<typeof Header> = () => (
  <Header mode="skeleton" />
);
