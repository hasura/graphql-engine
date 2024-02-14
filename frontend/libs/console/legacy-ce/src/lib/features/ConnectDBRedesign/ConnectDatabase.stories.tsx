import { Meta, StoryObj } from '@storybook/react';
import globals from '../../Globals';
import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { isCloudConsole } from '../../utils';
import { ConnectDatabaseV2 } from './ConnectDatabase';
import { useEnvironmentState } from './hooks';
import { handlers } from './mocks/handlers.mock';
import { ConsoleTypeDecorator } from '../../storybook/decorators';

export default {
  component: ConnectDatabaseV2,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers({ agentTestType: 'super_connector_agents_added' }),
  },
} as Meta<typeof ConnectDatabaseV2>;

const Template: StoryObj<typeof ConnectDatabaseV2> = {
  render: args => <ConnectDatabaseV2 {...args} />,
  args: {
    eeLicenseInfo: 'eligible',
    consoleType: 'pro-lite',
  },
};

export const FromEnvironment: StoryObj<typeof ConnectDatabaseV2> = {
  decorators: [ConsoleTypeDecorator({ consoleType: 'cloud-pro' })],
  render: () => {
    const env = useEnvironmentState();
    const cloud = isCloudConsole(globals);
    return (
      <div>
        <div className="my-3">
          This component attempts to read Console Type, and EE License Info from
          the environment
        </div>
        <div>is Cloud Console: {cloud.toString()}</div>
        <div>Console Type: {globals.consoleType}</div>
        <div>Tenant Id: {globals.hasuraCloudTenantId}</div>
        <ConnectDatabaseV2 {...env} />
      </div>
    );
  },

  name: '💠 Using Environment (DC Agents Added)',
};

export const FromEnvironment2 = {
  ...FromEnvironment,
  name: '💠 Using Environment (DC Agents NOT Added)',
  parameters: {
    msw: handlers({ agentTestType: 'super_connector_agents_not_added' }),
  },
};

export const Playground = {
  ...Template,
  name: '💠 Playground (DC Agents NOT Added)',
  parameters: {
    msw: handlers({ agentTestType: 'super_connector_agents_not_added' }),
  },
  args: Template.args,
};

export const Playground2 = {
  ...Template,
  name: '💠 Playground (DC Agents Added)',
};

export const Playground3 = {
  ...Template,
  name: '💠 Playground (DC Agents Added but not available)',
  parameters: {
    msw: handlers({
      agentTestType: 'super_connector_agents_added_but_unavailable',
    }),
  },
  args: Template.args,
};
