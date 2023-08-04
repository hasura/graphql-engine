import { Meta, StoryObj } from '@storybook/react';
import { ConsoleTypeDecorator } from '../../../../storybook/decorators';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { eeLicenseInfo } from '../../mocks/http';
import { registerEETrialLicenseActiveMutation } from '../../mocks/registration.mock';
import { SingleSignOnPage } from './SingleSignOnPage';

export default {
  title: 'features / EETrial / Single Sign On (SSO) Page 🧬️',
  component: SingleSignOnPage,
  decorators: [
    ReactQueryDecorator(),
    ConsoleTypeDecorator({ consoleType: 'pro-lite' }),
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
