import { Meta, StoryObj } from '@storybook/react';
import { SecurityTabs } from '../../../../components/Services/ApiExplorer/Security/SecurityTabs';
import { ConsoleTypeDecorator } from '../../../../storybook/decorators';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { eeLicenseInfo } from '../../mocks/http';
import { registerEETrialLicenseActiveMutation } from '../../mocks/registration.mock';
import { ApiSecurityTabEELiteWrapper } from './ApiSecurityTab';

export default {
  title: 'features / EETrial / API Security Tab 🧬️',
  component: ApiSecurityTabEELiteWrapper,
  decorators: [
    ReactQueryDecorator(),
    ConsoleTypeDecorator({ consoleType: 'pro-lite' }),
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
