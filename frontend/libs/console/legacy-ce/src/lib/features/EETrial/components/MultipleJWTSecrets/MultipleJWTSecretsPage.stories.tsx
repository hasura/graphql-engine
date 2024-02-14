import { Meta, StoryObj } from '@storybook/react';
import { ConsoleTypeDecorator } from '../../../../storybook/decorators';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { eeLicenseInfo } from '../../mocks/http';
import { registerEETrialLicenseActiveMutation } from '../../mocks/registration.mock';
import { MultipleJWTSecretsPage } from './MultipleJWTSecretsPage';

export default {
  title: 'features / EETrial / Multiple JWT Secrets Page 🧬️',
  component: MultipleJWTSecretsPage,
  decorators: [
    ReactQueryDecorator(),
    ConsoleTypeDecorator({ consoleType: 'pro-lite' }),
  ],
} as Meta<typeof MultipleJWTSecretsPage>;

export const Default: StoryObj<typeof MultipleJWTSecretsPage> = {
  render: () => {
    return <MultipleJWTSecretsPage />;
  },

  name: '💠 Default',

  parameters: {
    msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.none],
  },
};

export const LicenseActive: StoryObj<typeof MultipleJWTSecretsPage> = {
  render: () => {
    return <MultipleJWTSecretsPage />;
  },

  name: '💠 License Active',

  parameters: {
    msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.active],
  },
};

export const LicenseExpired: StoryObj<typeof MultipleJWTSecretsPage> = {
  render: () => {
    return <MultipleJWTSecretsPage />;
  },

  name: '💠 License Expired',

  parameters: {
    msw: [eeLicenseInfo.expired],
  },
};

export const LicenseDeactivated: StoryObj<typeof MultipleJWTSecretsPage> = {
  render: () => {
    return <MultipleJWTSecretsPage />;
  },

  name: '💠 License Deactivated',

  parameters: {
    msw: [eeLicenseInfo.deactivated],
  },
};
