import { StoryFn, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';

import { UntrackedFunctions } from './UntrackedFunctions';

export default {
  component: UntrackedFunctions,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof UntrackedFunctions>;

export const Primary: StoryFn<typeof UntrackedFunctions> = () => (
  <UntrackedFunctions dataSourceName="chinook" />
);
