import React from 'react';
import { StoryFn, Meta } from '@storybook/react';
import { Button } from '../../../../../../new-components/Button';

import { EmptyState } from './EmptyState';

export default {
  title: 'Features/Schema/EmptyState',
  component: EmptyState,
} as Meta<typeof EmptyState>;

export const EmptyStateStory: StoryFn = () => {
  return (
    <EmptyState
      title="It looks like you don't currently have any tables in your schema"
      subtitle="Need help getting started?"
      buttons={[
        <Button size="sm">Create</Button>,
        <Button size="sm" className="ml-sm">
          Delete
        </Button>,
      ]}
    />
  );
};
