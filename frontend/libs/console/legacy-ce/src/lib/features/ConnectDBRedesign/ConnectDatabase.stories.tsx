import { Meta, StoryObj } from '@storybook/react-webpack5';
import globals from '../../Globals';
import {
  ReactQueryDecorator,
  ConsoleTypeDecorator,
} from '@hasura/shared/testing';
import { ConnectDatabaseV2 } from './ConnectDatabase';
import { useEnvironmentState } from './hooks';
import { handlers } from './mocks/handlers.mock';

export default {
  component: ConnectDatabaseV2,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers({ agentTestType: 'super_connector_agents_added' }),
  },
} as Meta<typeof ConnectDatabaseV2>;

const Template: StoryObj<typeof ConnectDatabaseV2> = {
  render: (args) => <ConnectDatabaseV2 {...args} />,
  args: {
    eeLicenseInfo: 'eligible',
    consoleType: 'pro-lite',
  },
};

export const FromEnvironment: StoryObj<typeof ConnectDatabaseV2> = {
  decorators: [ConsoleTypeDecorator({ consoleType: 'cloud-pro' })],
  render: () => {
    const env = useEnvironmentState();
    const cloud = true;
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
