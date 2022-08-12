import React from 'react';
import { ComponentMeta, ComponentStory } from '@storybook/react';

const ExampleComponent: React.FC = ({ children }) => (
  <div className="p-4 border bg-gray-200 shadow">{children}</div>
);

export default {
  title: 'components/ExampleComponent',
  component: ExampleComponent,
} as ComponentMeta<typeof ExampleComponent>;

export const ExampleStory: ComponentStory<typeof ExampleComponent> = () => (
  <ExampleComponent>Test story</ExampleComponent>
);
