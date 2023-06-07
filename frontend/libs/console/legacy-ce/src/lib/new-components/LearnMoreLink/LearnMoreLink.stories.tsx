import type { Meta, StoryObj } from '@storybook/react';

import { LearnMoreLink } from './LearnMoreLink';

export default {
  title: 'components/LearnMoreLink ⚛️',
  component: LearnMoreLink,
  parameters: {
    docs: {
      description: {
        component: `Useful to link the docs of external resource containing more information`,
      },
      source: { type: 'code' },
    },
  },
} as Meta<typeof LearnMoreLink>;

export const Basic: StoryObj<typeof LearnMoreLink> = {
  name: '🧰 Basic',
  args: {
    href: 'https://hasura.io/docs',
  },
};

export const CustomText: StoryObj<typeof LearnMoreLink> = {
  name: '🎭 Custom text',
  args: {
    href: 'https://hasura.io/docs',
    text: '(Read the docs)',
  },
};
