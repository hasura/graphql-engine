import type { StoryFn } from '@storybook/react';
import * as React from 'react';

import { Header } from './Header';

export default {
  title: 'Features/OpenTelemetry/Header',
};

export const HeaderEnabled: StoryFn<typeof Header> = () => (
  <Header mode="enabled" />
);
export const HeaderDisabled: StoryFn<typeof Header> = () => (
  <Header mode="disabled" />
);
export const HeaderSkeleton: StoryFn<typeof Header> = () => (
  <Header mode="skeleton" />
);
