import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { Form } from './Form';

export default {
  title: 'Features/Data Relationships/Form',
  decorators: [ReactQueryDecorator()],
  component: Form,
} as ComponentMeta<typeof Form>;

export const Primary: ComponentStory<typeof Form> = args => <Form {...args} />;
Primary.args = {
  sourceTableInfo: {
    database: 'default',
    schema: 'public',
    table: 'resident',
  },
  driver: 'postgres',
};

export const WithExistingRelationship: ComponentStory<typeof Form> = args => (
  <Form {...args} />
);
WithExistingRelationship.args = {
  existingRelationship: {
    fromType: 'table',
    toType: 'table',
    name: 'products',
    reference: 'default',
    referenceTable: 'user',
    target: 'default',
    targetTable: 'product',
    type: 'Object',
    fieldsFrom: ['id'],
    fieldsTo: ['fk_user_id'],
    relationship: {
      name: 'products',
      using: {
        manual_configuration: {
          remote_table: 'product',
          column_mapping: { id: 'fk_user_id' },
        },
      },
    },
  },
  sourceTableInfo: {
    database: 'default',
    schema: 'public',
    table: 'resident',
  },
  driver: 'postgres',
};
