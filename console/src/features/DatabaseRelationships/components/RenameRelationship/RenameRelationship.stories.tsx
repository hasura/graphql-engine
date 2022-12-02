import React from 'react';

import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { RenameRelationship } from './RenameRelationship';
import { LocalRelationship } from '../../types';

export default {
  title: 'GDC Console/Rename Relationship',
  component: RenameRelationship,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof RenameRelationship>;

const demoRelationship: LocalRelationship = {
  name: 'order_items',
  fromSource: 'bikes',
  fromTable: {
    name: 'orders',
    schema: 'sales',
  },
  relationshipType: 'Array',
  type: 'localRelationship',
  definition: {
    toTable: {
      name: 'order_items',
      schema: 'sales',
    },
    mapping: {
      order_id: 'order_id',
    },
  },
};

export const Basic: ComponentStory<typeof RenameRelationship> = () => (
  <RenameRelationship
    relationship={demoRelationship}
    onCancel={() => {
      console.log('on cancel was triggered');
    }}
  />
);
