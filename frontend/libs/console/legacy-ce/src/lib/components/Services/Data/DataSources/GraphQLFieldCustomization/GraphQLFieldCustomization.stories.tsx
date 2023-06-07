import { StoryObj, Meta } from '@storybook/react';
import { GraphQLFieldCustomization } from './GraphQLFieldCustomization';

export default {
  title: 'data/components/DataSources/GraphQLFieldCustomization',
  component: GraphQLFieldCustomization,
  argTypes: { onChange: { action: 'change' } },
} as Meta<typeof GraphQLFieldCustomization>;

export const Playground: StoryObj<typeof GraphQLFieldCustomization> = {
  args: {
    rootFields: {
      namespace: '',
      prefix: '',
      suffix: '',
    },
    typeNames: {
      prefix: '',
      suffix: '',
    },
  },
};
