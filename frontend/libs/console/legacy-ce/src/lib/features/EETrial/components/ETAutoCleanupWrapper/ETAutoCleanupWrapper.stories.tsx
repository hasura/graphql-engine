import { Meta, StoryObj } from '@storybook/react';
import { AutoCleanupForm } from '../../../../components/Services/Events/EventTriggers/Common/AutoCleanupForm';
import { ConsoleTypeDecorator } from '../../../../storybook/decorators';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { eeLicenseInfo } from '../../mocks/http';
import { registerEETrialLicenseActiveMutation } from '../../mocks/registration.mock';
import { ETAutoCleanupWrapper } from './ETAutoCleanupWrapper';

export default {
  title: 'features / EETrial / Event Trigger Auto Cleanup Card 🧬️',
  component: ETAutoCleanupWrapper,
  decorators: [
    ReactQueryDecorator(),
    ConsoleTypeDecorator({ consoleType: 'pro-lite' }),
  ],
} as Meta<typeof ETAutoCleanupWrapper>;

export const Default: StoryObj<typeof ETAutoCleanupWrapper> = {
  render: () => {
    return (
      <ETAutoCleanupWrapper>
        <AutoCleanupForm onChange={() => {}} />
      </ETAutoCleanupWrapper>
    );
  },

  name: '💠 Default',

  parameters: {
    msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.none],
  },
};

export const LicenseActive: StoryObj<typeof ETAutoCleanupWrapper> = {
  render: () => {
    return (
      <ETAutoCleanupWrapper>
        <AutoCleanupForm onChange={() => {}} />
      </ETAutoCleanupWrapper>
    );
  },

  name: '💠 License Active',

  parameters: {
    msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.active],
  },
};

export const LicenseExpired: StoryObj<typeof ETAutoCleanupWrapper> = {
  render: () => {
    return (
      <ETAutoCleanupWrapper>
        <AutoCleanupForm onChange={() => {}} />
      </ETAutoCleanupWrapper>
    );
  },

  name: '💠 License Expired',

  parameters: {
    msw: [eeLicenseInfo.expired],
  },
};

export const LicenseDeactivated: StoryObj<typeof ETAutoCleanupWrapper> = {
  render: () => {
    return (
      <ETAutoCleanupWrapper>
        <AutoCleanupForm onChange={() => {}} />
      </ETAutoCleanupWrapper>
    );
  },

  name: '💠 License Deactivated',

  parameters: {
    msw: [eeLicenseInfo.deactivated],
  },
};
