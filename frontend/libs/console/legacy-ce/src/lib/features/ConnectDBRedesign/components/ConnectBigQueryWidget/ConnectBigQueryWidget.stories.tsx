import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ConnectBigQueryWidget } from './ConnectBigQueryWidget';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { handlers } from '../../mocks/handlers.mock';

export default {
  component: ConnectBigQueryWidget,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as ComponentMeta<typeof ConnectBigQueryWidget>;

export const CreateConnection: ComponentStory<
  typeof ConnectBigQueryWidget
> = () => {
  return (
    <div className="max-w-3xl">
      <ConnectBigQueryWidget />
    </div>
  );
};

export const EditConnection: ComponentStory<
  typeof ConnectBigQueryWidget
> = () => {
  return (
    <div className="max-w-3xl">
      <ConnectBigQueryWidget dataSourceName="test_source_bq" />
    </div>
  );
};
