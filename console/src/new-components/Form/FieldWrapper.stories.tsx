import React from 'react';
import { ComponentMeta } from '@storybook/react';
import { ComponentStory } from '@storybook/react/dist/ts3.9/client/preview/types-6-3';
import { FieldWrapper } from './FieldWrapper';

export default {
  title: 'components/Forms/FieldWrapper',
  component: FieldWrapper,
} as ComponentMeta<typeof FieldWrapper>;

export const Playground: ComponentStory<typeof FieldWrapper> = args => (
  <FieldWrapper {...args}>Children</FieldWrapper>
);

Playground.args = {
  children: <input className="border-black border" />,
  label: 'Label lorem input',
  error: {
    type: 'max',
    message: 'This is an error that also goes with the input field...',
  },
  description: 'Hello to you world',
  tooltip: 'Some tooltip to show on this input',
};

Playground.parameters = {
  backgrounds: {
    default: 'white',
    values: [{ name: 'white', value: '#fff' }],
  },
};
