import { StoryFn, Meta } from '@storybook/react-webpack5';
import { ReactQueryDecorator } from '@hasura/shared/testing';

import { UntrackedFunctions } from './UntrackedFunctions';

export default {
  component: UntrackedFunctions,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof UntrackedFunctions>;

export const Primary: StoryFn<typeof UntrackedFunctions> = () => (
  <UntrackedFunctions dataSourceName="chinook" untrackedFunctions={[]} />
);
