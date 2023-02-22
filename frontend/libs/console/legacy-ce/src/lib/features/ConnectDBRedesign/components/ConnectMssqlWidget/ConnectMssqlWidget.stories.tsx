import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ConnectMssqlWidget } from './ConnectMssqlWidget';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { handlers } from '../../mocks/handlers.mock';

export default {
  component: ConnectMssqlWidget,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as ComponentMeta<typeof ConnectMssqlWidget>;

export const CreateConnection: ComponentStory<
  typeof ConnectMssqlWidget
> = () => {
  return (
    <div className="max-w-3xl">
      <ConnectMssqlWidget />
    </div>
  );
};

export const EditConnection: ComponentStory<typeof ConnectMssqlWidget> = () => {
  return (
    <div className="max-w-3xl">
      <ConnectMssqlWidget dataSourceName="mssql1" />
    </div>
  );
};
