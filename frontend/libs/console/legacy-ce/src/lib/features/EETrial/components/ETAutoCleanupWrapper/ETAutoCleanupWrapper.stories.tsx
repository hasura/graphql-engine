import { Meta, StoryObj, Decorator } from '@storybook/react-webpack5';
import { AutoCleanupForm } from '../../../Eventing/EventTriggers/components/form/AutoCleanupForm';
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
import { ETAutoCleanupWrapper } from './ETAutoCleanupWrapper';

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
  title: 'features / EETrial / Event Trigger Auto Cleanup Card 🧬️',
  component: ETAutoCleanupWrapper,
  decorators: [
    ReactQueryDecorator(),
    ConsoleTypeDecorator({ consoleType: 'pro-lite' }),
    AppContextDecorator,
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
