import React from 'react';
import { ComponentMeta } from '@storybook/react';
import { ComponentStory } from '@storybook/react/dist/ts3.9/client/preview/types-6-3';
import { GraphQLFieldCustomization } from './GraphQLFieldCustomization';

export default {
  title: 'data/components/DataSources/GraphQLFieldCustomization',
  component: GraphQLFieldCustomization,
  argTypes: { onChange: { action: 'change' } },
} as ComponentMeta<typeof GraphQLFieldCustomization>;

export const Playground: ComponentStory<typeof GraphQLFieldCustomization> =
  args => <GraphQLFieldCustomization {...args} />;

Playground.args = {
  rootFields: {
    namespace: '',
    prefix: '',
    suffix: '',
  },
  typeNames: {
    prefix: '',
    suffix: '',
  },
};
