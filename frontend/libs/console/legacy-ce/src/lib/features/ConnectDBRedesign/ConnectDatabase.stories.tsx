import { ComponentMeta, ComponentStory } from '@storybook/react';
import globals from '../../Globals';
import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { isCloudConsole } from '../../utils';
import { ConnectDatabaseV2 } from './ConnectDatabase';
import { useEnvironmentState } from './hooks';
import { handlers } from './mocks/handlers.mock';

export default {
  component: ConnectDatabaseV2,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers({ agentTestType: 'super_connector_agents_added' }),
  },
} as ComponentMeta<typeof ConnectDatabaseV2>;

const Template: ComponentStory<typeof ConnectDatabaseV2> = args => {
  return <ConnectDatabaseV2 {...args} />;
};

Template.args = {
  eeLicenseInfo: 'eligible',
  consoleType: 'pro-lite',
};

/**
 *
 * This Story attempts to get oss/cloud/license info from environment.
 *
 * DC Agents are mocked as available
 *
 * TODO: Add mocks for licensing api calls.
 *
 * The new Storybook Console Mode drop down can be used to interact with this version
 *
 */

export const FromEnvironment: ComponentStory<typeof ConnectDatabaseV2> = () => {
  const env = useEnvironmentState();
  const cloud = isCloudConsole(globals);
  return (
    <div>
      <div className="my-3">
        This component attempts to read Console Type, and EE License Info from
        the environment
      </div>
      <div>isCloud: {cloud.toString()}</div>
      <div>Console Type: {globals.consoleType}</div>
      <div>Tenant Id: {globals.hasuraCloudTenantId}</div>
      <ConnectDatabaseV2 {...env} />
    </div>
  );
};
FromEnvironment.storyName = '💠 Using Environment (DC Agents Added)';

/**
 *
 * This Story attempts to get oss/cloud/license info from environment.
 *
 * DC Agents are mocked as NOT available
 *
 *
 * The new Storybook Console Mode drop down can be used to interact with this version
 *
 */

export const FromEnvironment2 = FromEnvironment.bind({});
FromEnvironment2.storyName = '💠 Using Environment (DC Agents NOT Added)';
FromEnvironment2.parameters = {
  msw: handlers({ agentTestType: 'super_connector_agents_not_added' }),
};
/**
 *
 * Playground
 *
 * Mock DC Agents are NOT added in this version
 *
 */
export const Playground = Template.bind({});
Playground.storyName = '💠 Playground (DC Agents NOT Added)';
Playground.parameters = {
  msw: handlers({ agentTestType: 'super_connector_agents_not_added' }),
};
Playground.args = Template.args;

/**
 *
 * Playground 2
 *
 *
 * Mock DC Agents are added in this version
 *
 *
 */

export const Playground2 = Template.bind({});
Playground2.storyName = '💠 Playground (DC Agents Added)';
Playground2.args = Template.args;

/**
 *
 * Playground 3
 *
 *
 * Mock DC Agents are added in this version
 *
 *
 */

export const Playground3 = Template.bind({});
Playground3.storyName = '💠 Playground (DC Agents Added but not available)';
Playground3.parameters = {
  msw: handlers({
    agentTestType: 'super_connector_agents_added_but_unavailable',
  }),
};
Playground3.args = Template.args;

/**
 * TODO:
 *
 * Re-write old tests to work with refactored component....
 *
 *
 */

// const defaultArgs: ConnectDatabaseProps = {
//   environmentState: {
//     eeLicenseInfo: 'active',
//     isCloud: false,
//     isOss: false,
//     isPro: false,
//   },
// };

// export const No_Enterprise_Drivers = Template.bind({});
// No_Enterprise_Drivers.storyName = 'No Enterprise Drivers (OSS)';
// No_Enterprise_Drivers.args = {
//   ...defaultArgs,
// };

// No_Enterprise_Drivers.play = async ({ canvasElement }) => {
//   const c = within(canvasElement);
//   await waitFor(() => c.findByTestId('fancy-radio-postgres'));
//   await expect(
//     c.queryByTestId('fancy-radio-snowflake')
//   ).not.toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-athena')).not.toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-sqlite')).toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-mysql8')).toBeInTheDocument();
// };

// export const License_Inactive = Template.bind({});
// License_Inactive.args = {
//   ...defaultArgs,
//   showEnterpriseDrivers: true,
//   licenseState: 'forbidden',
// };

// License_Inactive.play = async ({ canvasElement }) => {
//   const c = within(canvasElement);
//   await waitFor(() => c.findByTestId('fancy-radio-postgres'));

//   await expect(c.queryByTestId('fancy-radio-snowflake')).toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-athena')).toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-sqlite')).toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-mysql8')).toBeInTheDocument();

//   await userEvent.click(await c.findByTestId('fancy-label-snowflake'));

//   await expect(
//     await c.findByTestId('license-inactive-card')
//   ).toBeInTheDocument();

//   await expect(
//     c.queryByTestId('connect-existing-button')
//   ).not.toBeInTheDocument();
// };

// export const License_Active_GDC_N = Template.bind({});
// License_Active_GDC_N.storyName = 'License Active: DC Agents Not Added';
// License_Active_GDC_N.args = {
//   ...defaultArgs,
//   showEnterpriseDrivers: true,
//   licenseState: 'active',
//   initialDriverName: 'snowflake',
// };

// License_Active_GDC_N.play = async ({ canvasElement }) => {
//   const c = within(canvasElement);
//   await waitFor(() => c.findByTestId('fancy-radio-postgres'));

//   await expect(c.queryByTestId('fancy-radio-snowflake')).toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-athena')).toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-sqlite')).toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-mysql8')).toBeInTheDocument();

//   await userEvent.click(await c.findByTestId('fancy-label-snowflake'));

//   await expect(
//     await c.findByTestId('setup-data-connector-card')
//   ).toBeInTheDocument();

//   await expect(
//     c.queryByTestId('connect-existing-button')
//   ).not.toBeInTheDocument();
// };

// export const License_Active_GDC_Y = Template.bind({});
// License_Active_GDC_Y.parameters = {
//   msw: handlers({ dcAgentsAdded: true }),
// };
// License_Active_GDC_Y.storyName = 'License Active: DC Agents Added';
// License_Active_GDC_Y.args = {
//   ...defaultArgs,
//   showEnterpriseDrivers: true,
//   licenseState: 'active',
//   initialDriverName: 'snowflake',
// };

// License_Active_GDC_Y.play = async ({ canvasElement }) => {
//   const c = within(canvasElement);
//   await waitFor(() => c.findByTestId('fancy-radio-postgres'));

//   await expect(c.queryByTestId('fancy-radio-snowflake')).toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-athena')).toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-sqlite')).toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-mysql8')).toBeInTheDocument();

//   await userEvent.click(await c.findByTestId('fancy-label-snowflake'));

//   await expect(
//     c.queryByTestId('setup-data-connector-card')
//   ).not.toBeInTheDocument();
//   await expect(c.queryByTestId('connect-existing-button')).toBeInTheDocument();
// };

// export const License_Expired = Template.bind({});
// License_Expired.args = {
//   ...defaultArgs,
//   showEnterpriseDrivers: true,
//   licenseState: 'expired',
//   initialDriverName: 'snowflake',
// };

// License_Expired.play = async ({ canvasElement }) => {
//   const c = within(canvasElement);
//   await waitFor(() => c.findByTestId('fancy-radio-postgres'));

//   await expect(c.queryByTestId('fancy-radio-snowflake')).toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-athena')).toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-sqlite')).toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-mysql8')).toBeInTheDocument();
//   await userEvent.click(await c.findByTestId('fancy-label-snowflake'));
//   await expect(
//     await c.findByTestId('license-expired-card')
//   ).toBeInTheDocument();
//   await expect(
//     await c.queryByTestId('connect-existing-button')
//   ).not.toBeInTheDocument();
// };

// export const License_Deactivated = Template.bind({});
// License_Deactivated.args = {
//   ...defaultArgs,
//   showEnterpriseDrivers: true,
//   licenseState: 'deactivated',
//   initialDriverName: 'snowflake',
// };

// License_Deactivated.play = License_Expired.play;

// export const Cloud_GDC_Y = Template.bind({});
// Cloud_GDC_Y.storyName = 'Cloud: DC Agents Available';
// Cloud_GDC_Y.parameters = {
//   msw: handlers({ dcAgentsAdded: true }),
// };
// Cloud_GDC_Y.args = {
//   ...defaultArgs,
//   dataConnectorHostType: 'cloud',
//   showEnterpriseDrivers: true,
//   initialDriverName: 'snowflake',
// };

// Cloud_GDC_Y.play = async ({ canvasElement }) => {
//   const c = within(canvasElement);
//   await waitFor(() => c.findByTestId('fancy-radio-postgres'));

//   await expect(c.queryByTestId('fancy-radio-snowflake')).toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-athena')).toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-sqlite')).toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-mysql8')).toBeInTheDocument();
//   await userEvent.click(await c.findByTestId('fancy-label-snowflake'));
//   await expect(
//     await c.queryByTestId('cloud-driver-not-available')
//   ).not.toBeInTheDocument();
//   await expect(
//     await c.queryByTestId('connect-existing-button')
//   ).toBeInTheDocument();
// };

// export const Cloud_GDC_N = Template.bind({});
// Cloud_GDC_N.storyName = 'Cloud: DC Agents Not Available';
// Cloud_GDC_N.parameters = {
//   msw: handlers({ dcAgentsAdded: false }),
// };
// Cloud_GDC_N.args = {
//   ...defaultArgs,
//   dataConnectorHostType: 'cloud',
//   showEnterpriseDrivers: true,
//   initialDriverName: 'snowflake',
// };

// Cloud_GDC_N.play = async ({ canvasElement }) => {
//   const c = within(canvasElement);
//   await waitFor(() => c.findByTestId('fancy-radio-postgres'));

//   await expect(c.queryByTestId('fancy-radio-snowflake')).toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-athena')).toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-sqlite')).toBeInTheDocument();
//   await expect(c.queryByTestId('fancy-radio-mysql8')).toBeInTheDocument();
//   await userEvent.click(await c.findByTestId('fancy-label-snowflake'));
//   await expect(
//     await c.findByTestId('cloud-driver-not-available')
//   ).toBeInTheDocument();
//   await expect(
//     await c.queryByTestId('connect-existing-button')
//   ).not.toBeInTheDocument();
// };

// export const Neon_Connect = Template.bind({});
// Neon_Connect.args = {
//   ...defaultArgs,
//   allowNeonConnect: true,
// };

// Neon_Connect.play = async ({ canvasElement }) => {
//   const c = within(canvasElement);
//   await waitFor(() => c.findByTestId('fancy-radio-postgres'));

//   await userEvent.click(await c.findByTestId('fancy-label-postgres'));
//   await expect(await c.findByTestId('neon-connect')).toBeInTheDocument();
//   await expect(
//     await c.findByTestId('connect-existing-button')
//   ).toBeInTheDocument();
// };
