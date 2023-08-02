import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { AutoCleanupForm } from '../../../../components/Services/Events/EventTriggers/Common/AutoCleanupForm';
import {
  registerEETrialLicenseActiveMutation,
  registerEETrialLicenseDeactivatedMutation,
  registerEETrialLicenseExpiredMutation,
} from '../../mocks/registration.mock';
import { ETAutoCleanupWrapper } from './ETAutoCleanupWrapper';
import { eeLicenseInfo } from '../../mocks/http';
import { EE_LICENSE_INFO_QUERY_NAME } from '../../constants';
import { useQueryClient } from 'react-query';

export default {
  title: 'features / EETrial / Event Trigger Auto Cleanup Card 🧬️',
  component: ETAutoCleanupWrapper,
  decorators: [
    // This is done so as we have set some cache time on the EE_LICENSE_INFO_QUERY_NAME query.
    // So we need to refetch the cache data, so it doesn't persist across different stories. And
    // it makes sure that our component actually does the network call, letting msw mocks return the
    // desired response.
    Story => {
      const queryClient = useQueryClient();
      void queryClient.refetchQueries(EE_LICENSE_INFO_QUERY_NAME);
      return <Story />;
    },
    ReactQueryDecorator(),
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
    consoleType: 'pro-lite',
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
    consoleType: 'pro-lite',
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
    msw: [registerEETrialLicenseExpiredMutation, eeLicenseInfo.expired],
    consoleType: 'pro-lite',
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
    msw: [registerEETrialLicenseDeactivatedMutation, eeLicenseInfo.deactivated],
    consoleType: 'pro-lite',
  },
};
