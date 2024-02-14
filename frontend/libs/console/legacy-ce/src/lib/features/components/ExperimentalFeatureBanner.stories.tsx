import { Meta, StoryObj } from '@storybook/react';
import { ExperimentalFeatureBanner } from './ExperimentalFeatureBanner';

export default {
  title: 'features/components/Experimental Feature Banner 🧬',
  component: ExperimentalFeatureBanner,
  args: {
    githubIssueLink: 'https://github.com',
  },
} as Meta<typeof ExperimentalFeatureBanner>;

export const Basic: StoryObj<typeof ExperimentalFeatureBanner> = {
  render: args => <ExperimentalFeatureBanner {...args} />,
};
