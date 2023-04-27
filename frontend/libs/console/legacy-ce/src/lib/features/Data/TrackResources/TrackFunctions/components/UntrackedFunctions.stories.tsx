import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';

import { UntrackedFunctions } from './UntrackedFunctions';

export default {
  component: UntrackedFunctions,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof UntrackedFunctions>;

export const Primary: ComponentStory<typeof UntrackedFunctions> = () => (
  <UntrackedFunctions dataSourceName="chinook" />
);
