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
    name: 'Customers',
    type: 'toLocalTableFk',
    toLocalTable: {
      name: 'Customer',
      schema: 'public',
    },
    relationship_type: 'Array',
    mapping: {
      from: {
        source: 'chinook',
        table: {
          name: 'Employee',
          schema: 'public',
        },
        columns: ['SupportRepId'],
      },
      to: {
        source: 'chinook',
        table: {
          name: 'Customer',
          schema: 'public',
        },
        columns: ['EmployeeId'],
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
