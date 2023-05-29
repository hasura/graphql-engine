import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { ConnectDatabaseSidebar } from './ConnectDatabaseSidebar';
import { handlers } from '../mocks/handlers.mock';

export default {
  component: ConnectDatabaseSidebar,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers({ dcAgentsAdded: false }),
  },
} as Meta<typeof ConnectDatabaseSidebar>;

export const Primary: StoryObj<typeof ConnectDatabaseSidebar> = {
  render: args => (
    <div className="max-w-lg">
      <ConnectDatabaseSidebar {...args} />
    </div>
  ),

  args: {
    //licenseState: 'none',
    //hostType: 'self-hosted',
    //allowNeonConnect: false,
    //showEnterpriseDrivers: true,
  },
};
