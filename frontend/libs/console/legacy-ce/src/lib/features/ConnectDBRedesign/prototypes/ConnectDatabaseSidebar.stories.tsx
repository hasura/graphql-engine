import { ComponentMeta, ComponentStory } from '@storybook/react';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { ConnectDatabaseSidebar } from './ConnectDatabaseSidebar';
import { handlers } from '../mocks/handlers.mock';

export default {
  component: ConnectDatabaseSidebar,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers({ dcAgentsAdded: false }),
  },
} as ComponentMeta<typeof ConnectDatabaseSidebar>;

export const Primary: ComponentStory<typeof ConnectDatabaseSidebar> = args => (
  <div className="max-w-lg">
    <ConnectDatabaseSidebar {...args} />
  </div>
);

Primary.args = {
  //licenseState: 'none',
  //hostType: 'self-hosted',
  //allowNeonConnect: false,
  //showEnterpriseDrivers: true,
};
