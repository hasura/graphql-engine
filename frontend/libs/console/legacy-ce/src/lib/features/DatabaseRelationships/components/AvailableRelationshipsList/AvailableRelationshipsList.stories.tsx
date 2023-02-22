import React from 'react';

import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { AvailableRelationshipsList } from './AvailableRelationshipsList';

export default {
  component: AvailableRelationshipsList,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof AvailableRelationshipsList>;

export const Primary: ComponentStory<
  typeof AvailableRelationshipsList
> = () => (
  <AvailableRelationshipsList
    dataSourceName="chinook"
    table={{ name: 'Album', schema: 'public' }}
    onAction={data => console.log(data)}
  />
);
