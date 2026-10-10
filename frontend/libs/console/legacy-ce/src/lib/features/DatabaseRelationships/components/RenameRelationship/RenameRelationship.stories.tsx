import React from 'react';

import { StoryFn, Meta } from '@storybook/react-webpack5';
import { ReactQueryDecorator } from '@hasura/shared/testing';
import { RenameRelationship } from './RenameRelationship';
import { LocalRelationship } from '../../types';

export default {
  component: RenameRelationship,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof RenameRelationship>;

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

export const Basic: StoryFn<typeof RenameRelationship> = () => (
  <RenameRelationship
    relationship={demoRelationship}
    onCancel={() => {
      console.log('on cancel was triggered');
    }}
  />
);
