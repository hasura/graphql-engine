import React from 'react';
import { Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { FeatureFlags } from './FeatureFlags';
import { FeatureFlagDefinition } from '../../types';

export default {
  title: 'features/FeatureFlags',
  component: FeatureFlags,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof FeatureFlags>;

const additionalFlags: FeatureFlagDefinition[] = [
  {
    id: '1',
    title: 'Database Table → Remote Schema Relationship',
    description:
      "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book.",
    section: 'api',
    status: 'alpha',
    defaultValue: false,
    discussionUrl: '',
  },
  {
    id: '2',
    title: 'Database Table → Remote Schema Relationship',
    description:
      "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book.",
    section: 'api',
    status: 'alpha',
    defaultValue: true,
    discussionUrl: '',
  },
  {
    id: '3',
    title: 'Database Table → Remote Schema Relationship',
    description:
      "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book.",
    section: 'api',
    status: 'alpha',
    defaultValue: false,
    discussionUrl: '',
  },
];

export const Main = () => <FeatureFlags additionalFlags={additionalFlags} />;

export const EmptyState = () => <FeatureFlags />;
