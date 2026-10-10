import { Meta, StoryObj, Decorator } from '@storybook/react-webpack5';
import {
  ConsoleTypeDecorator,
  ReactQueryDecorator,
} from '@hasura/shared/testing';
import {
  AppContext,
  defaultAppState,
  getEndpoints,
} from '@hasura/shared/context';
import { eeLicenseInfo } from '../../mocks/http';
import { registerEETrialLicenseActiveMutation } from '../../mocks/registration.mock';
import { SingleSignOnPage } from './SingleSignOnPage';

// re-reads window.__env on every render so it picks up ConsoleTypeDecorator's updates
const AppContextDecorator: Decorator = (Story) => (
  <AppContext.Provider
    value={{
      ...defaultAppState,
      envVars: window.__env ?? {},
      endpoints: getEndpoints(window.__env ?? {}, 'http://localhost'),
    }}
  >
    <Story />
  </AppContext.Provider>
);

export default {
  title: 'features / EETrial / Single Sign On (SSO) Page 🧬️',
  component: SingleSignOnPage,
  decorators: [
    ReactQueryDecorator(),
    ConsoleTypeDecorator({ consoleType: 'pro-lite' }),
    AppContextDecorator,
  ],
} as Meta<typeof SingleSignOnPage>;

export const Default: StoryObj<typeof SingleSignOnPage> = {
  render: () => {
    return <SingleSignOnPage />;
  },

  name: '💠 Default',

  parameters: {
    msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.none],
  },
};

export const LicenseActive: StoryObj<typeof SingleSignOnPage> = {
  render: () => {
    return <SingleSignOnPage />;
  },

  name: '💠 License Active',

  parameters: {
    msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.active],
  },
};

export const LicenseExpired: StoryObj<typeof SingleSignOnPage> = {
  render: () => {
    return <SingleSignOnPage />;
  },

  name: '💠 License Expired',

  parameters: {
    msw: [eeLicenseInfo.expired],
  },
};

export const LicenseDeactivated: StoryObj<typeof SingleSignOnPage> = {
  render: () => {
    return <SingleSignOnPage />;
  },

  name: '💠 License Deactivated',

  parameters: {
    msw: [eeLicenseInfo.deactivated],
  },
};
