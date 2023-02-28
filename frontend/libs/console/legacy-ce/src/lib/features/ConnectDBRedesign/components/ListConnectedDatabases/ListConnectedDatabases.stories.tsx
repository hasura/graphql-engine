import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { handlers } from '../../mocks/handlers.mock';

import { ListConnectedDatabases } from './ListConnectedDatabases';

export default {
  component: ListConnectedDatabases,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as ComponentMeta<typeof ListConnectedDatabases>;

export const Primary: ComponentStory<typeof ListConnectedDatabases> = () => (
  <ListConnectedDatabases />
);
