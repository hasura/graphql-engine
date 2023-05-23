import { ComponentMeta, ComponentStory } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { ListStoredProcedures } from './ListStoredProcedures';
import { handlers } from '../../LogicalModelWidget/mocks/handlers';

export default {
  component: ListStoredProcedures,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof ListStoredProcedures>;

export const Basic: ComponentStory<typeof ListStoredProcedures> = args => {
  return <ListStoredProcedures />;
};

Basic.parameters = {
  msw: handlers['200'],
};
