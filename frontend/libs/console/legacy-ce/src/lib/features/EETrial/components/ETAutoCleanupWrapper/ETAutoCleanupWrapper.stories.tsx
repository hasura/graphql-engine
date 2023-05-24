import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
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
} as ComponentMeta<typeof ETAutoCleanupWrapper>;

export const Default: ComponentStory<typeof ETAutoCleanupWrapper> = () => {
  return (
    <ETAutoCleanupWrapper>
      <AutoCleanupForm onChange={() => {}} />
    </ETAutoCleanupWrapper>
  );
};
Default.storyName = '💠 Default';
Default.parameters = {
  msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.none],
  consoleType: 'pro-lite',
};

export const LicenseActive: ComponentStory<
  typeof ETAutoCleanupWrapper
> = () => {
  return (
    <ETAutoCleanupWrapper>
      <AutoCleanupForm onChange={() => {}} />
    </ETAutoCleanupWrapper>
  );
};
LicenseActive.storyName = '💠 License Active';
LicenseActive.parameters = {
  msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.active],
  consoleType: 'pro-lite',
};

export const LicenseExpired: ComponentStory<
  typeof ETAutoCleanupWrapper
> = () => {
  return (
    <ETAutoCleanupWrapper>
      <AutoCleanupForm onChange={() => {}} />
    </ETAutoCleanupWrapper>
  );
};
LicenseExpired.storyName = '💠 License Expired';
LicenseExpired.parameters = {
  msw: [registerEETrialLicenseExpiredMutation, eeLicenseInfo.expired],
  consoleType: 'pro-lite',
};

export const LicenseDeactivated: ComponentStory<
  typeof ETAutoCleanupWrapper
> = () => {
  return (
    <ETAutoCleanupWrapper>
      <AutoCleanupForm onChange={() => {}} />
    </ETAutoCleanupWrapper>
  );
};
LicenseDeactivated.storyName = '💠 License Deactivated';
LicenseDeactivated.parameters = {
  msw: [registerEETrialLicenseDeactivatedMutation, eeLicenseInfo.deactivated],
  consoleType: 'pro-lite',
};
