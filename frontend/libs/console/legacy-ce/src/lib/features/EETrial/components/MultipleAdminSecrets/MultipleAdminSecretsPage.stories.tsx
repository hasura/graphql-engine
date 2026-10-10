import { Meta, StoryObj } from '@storybook/react-webpack5';
import {
  ConsoleTypeDecorator,
  ReactQueryDecorator,
} from '@hasura/shared/testing';
import { eeLicenseInfo } from '../../mocks/http';
import { registerEETrialLicenseActiveMutation } from '../../mocks/registration.mock';
import { MultipleAdminSecretsPage } from './MultipleAdminSecretsPage';

export default {
  title: 'features / EETrial / Multiple Admin Secrets Page 🧬️',
  component: MultipleAdminSecretsPage,
  decorators: [
    ReactQueryDecorator(),
    ConsoleTypeDecorator({ consoleType: 'pro-lite' }),
  ],
} as Meta<typeof MultipleAdminSecretsPage>;

export const Default: StoryObj<typeof MultipleAdminSecretsPage> = {
  render: () => {
    return <MultipleAdminSecretsPage />;
  },

  name: '💠 Default',

  parameters: {
    msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.none],
  },
};

export const LicenseActive: StoryObj<typeof MultipleAdminSecretsPage> = {
  render: () => {
    return <MultipleAdminSecretsPage />;
  },

  name: '💠 License Active',

  parameters: {
    msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.active],
  },
};

export const LicenseExpired: StoryObj<typeof MultipleAdminSecretsPage> = {
  render: () => {
    return <MultipleAdminSecretsPage />;
  },

  name: '💠 License Expired',

  parameters: {
    msw: [eeLicenseInfo.expired],
  },
};

export const LicenseDeactivated: StoryObj<typeof MultipleAdminSecretsPage> = {
  render: () => {
    return <MultipleAdminSecretsPage />;
  },

  name: '💠 License Deactivated',

  parameters: {
    msw: [eeLicenseInfo.deactivated],
  },
};
