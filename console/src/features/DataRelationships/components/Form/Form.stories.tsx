import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { Form } from './Form';

export default {
  title: 'Features/Data Relationships/Form',
  decorators: [ReactQueryDecorator()],
  component: Form,
} as ComponentMeta<typeof Form>;

export const Primary: ComponentStory<typeof Form> = () => (
  <Form
    sourceTableInfo={{
      database: 'default',
      schema: 'public',
      table: 'resident',
    }}
    driver="postgres"
    onComplete={console.log}
  />
);
