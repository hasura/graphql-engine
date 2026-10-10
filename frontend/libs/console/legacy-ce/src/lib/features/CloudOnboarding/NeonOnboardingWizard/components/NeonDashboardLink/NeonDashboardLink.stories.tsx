import { StoryFn, Meta } from '@storybook/react-webpack5';
import { NeonDashboardLink } from './NeonDashboardLink';
import { ReactQueryDecorator } from '@hasura/shared/testing';

export default {
  title: 'features/Neon Integration/Neon Dashboard Link',
  component: NeonDashboardLink,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof NeonDashboardLink>;

export const Base: StoryFn = () => <NeonDashboardLink />;
