import { StoryFn, Meta } from '@storybook/react';
import { ConnectGDCSourceWidget } from './ConnectGDCSourceWidget';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { handlers } from '../../mocks/handlers.mock';

export default {
  component: ConnectGDCSourceWidget,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers({ agentTestType: 'super_connector_agents_added' }),
  },
} as Meta<typeof ConnectGDCSourceWidget>;

export const CreateConnection: StoryFn<typeof ConnectGDCSourceWidget> = () => {
  return (
    <div className="max-w-5xl">
      <ConnectGDCSourceWidget driver="sqlite" />
    </div>
  );
};

export const EditConnection: StoryFn<typeof ConnectGDCSourceWidget> = () => {
  return (
    <div className="max-w-5xl">
      <ConnectGDCSourceWidget driver="sqlite" dataSourceName="sqlite_test" />
    </div>
  );
};
