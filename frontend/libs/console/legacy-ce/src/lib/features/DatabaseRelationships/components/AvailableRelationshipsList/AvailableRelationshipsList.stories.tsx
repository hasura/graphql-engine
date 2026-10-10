import React from 'react';

import { StoryFn, Meta } from '@storybook/react-webpack5';
import { ReactQueryDecorator } from '@hasura/shared/testing';
import { AvailableRelationshipsList } from './AvailableRelationshipsList';

export default {
  component: AvailableRelationshipsList,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof AvailableRelationshipsList>;

export const Primary: StoryFn<typeof AvailableRelationshipsList> = () => (
  <AvailableRelationshipsList
    dataSourceName="chinook"
    table={{ name: 'Album', schema: 'public' }}
    onAction={(data) => console.log(data)}
  />
);
