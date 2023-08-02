import React from 'react';

import { StoryFn, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { AvailableRelationshipsList } from './AvailableRelationshipsList';

export default {
  component: AvailableRelationshipsList,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof AvailableRelationshipsList>;

export const Primary: StoryFn<typeof AvailableRelationshipsList> = () => (
  <AvailableRelationshipsList
    dataSourceName="chinook"
    table={{ name: 'Album', schema: 'public' }}
    onAction={data => console.log(data)}
  />
);
