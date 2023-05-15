import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ConnectGDCSourceWidget } from './ConnectGDCSourceWidget';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { handlers } from '../../mocks/handlers.mock';

export default {
  component: ConnectGDCSourceWidget,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers({ agentTestType: 'super_connector_agents_not_added' }),
  },
} as ComponentMeta<typeof ConnectGDCSourceWidget>;

export const CreateConnection: ComponentStory<
  typeof ConnectGDCSourceWidget
> = () => {
  return (
    <div className="max-w-3xl">
      <ConnectGDCSourceWidget driver="sqlite" />
    </div>
  );
};

export const EditConnection: ComponentStory<
  typeof ConnectGDCSourceWidget
> = () => {
  return (
    <div className="max-w-3xl">
      <ConnectGDCSourceWidget driver="sqlite" dataSourceName="sqlite_test" />
    </div>
  );
};
