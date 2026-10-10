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
import { ApiSecurityTabEELiteWrapper } from './ApiSecurityTab';
import { SecurityTabs } from '../../../ApiExplorer/components/Security/SecurityTabs';
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
  title: 'features / EETrial / API Security Tab 🧬️',
  component: ApiSecurityTabEELiteWrapper,
  decorators: [
    ReactQueryDecorator(),
    ConsoleTypeDecorator({ consoleType: 'pro-lite' }),
    AppContextDecorator,
  ],
} as Meta<typeof ApiSecurityTabEELiteWrapper>;

export const Default: StoryObj<typeof ApiSecurityTabEELiteWrapper> = {
  render: () => {
    return (
      <ApiSecurityTabEELiteWrapper>
        <SecurityTabs tabName="api_limits" />
      </ApiSecurityTabEELiteWrapper>
    );
  },

  name: '💠 Default',

  parameters: {
    msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.none],
  },
};

export const LicenseActive: StoryObj<typeof ApiSecurityTabEELiteWrapper> = {
  render: () => {
    return (
      <ApiSecurityTabEELiteWrapper>
        <SecurityTabs tabName="api_limits" />
      </ApiSecurityTabEELiteWrapper>
    );
  },

  name: '💠 License Active',

  parameters: {
    msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.active],
  },
};

export const LicenseExpired: StoryObj<typeof ApiSecurityTabEELiteWrapper> = {
  render: () => {
    return (
      <ApiSecurityTabEELiteWrapper>
        <SecurityTabs tabName="api_limits" />
      </ApiSecurityTabEELiteWrapper>
    );
  },

  name: '💠 License Expired',

  parameters: {
    msw: [eeLicenseInfo.expired],
  },
};

export const LicenseDeactivated: StoryObj<typeof ApiSecurityTabEELiteWrapper> =
  {
    render: () => {
      return (
        <ApiSecurityTabEELiteWrapper>
          <SecurityTabs tabName="api_limits" />
        </ApiSecurityTabEELiteWrapper>
      );
    },

    name: '💠 License Deactivated',

    parameters: {
      msw: [eeLicenseInfo.deactivated],
    },
  };
