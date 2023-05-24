import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { registerEETrialLicenseActiveMutation } from '../../mocks/registration.mock';
import { SingleSignOnPage } from './SingleSignOnPage';
import { eeLicenseInfo } from '../../mocks/http';
import { useQueryClient } from 'react-query';
import { EE_LICENSE_INFO_QUERY_NAME } from '../../constants';

export default {
  title: 'features / EETrial / Single Sign On (SSO) Page 🧬️',
  component: SingleSignOnPage,
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
} as ComponentMeta<typeof SingleSignOnPage>;

export const Default: ComponentStory<typeof SingleSignOnPage> = () => {
  return <SingleSignOnPage />;
};
Default.storyName = '💠 Default';
Default.parameters = {
  msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.none],
  consoleType: 'pro-lite',
};

export const LicenseActive: ComponentStory<typeof SingleSignOnPage> = () => {
  return <SingleSignOnPage />;
};
LicenseActive.storyName = '💠 License Active';
LicenseActive.parameters = {
  msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.active],
  consoleType: 'pro-lite',
};

export const LicenseExpired: ComponentStory<typeof SingleSignOnPage> = () => {
  return <SingleSignOnPage />;
};
LicenseExpired.storyName = '💠 License Expired';
LicenseExpired.parameters = {
  msw: [eeLicenseInfo.expired],
  consoleType: 'pro-lite',
};

export const LicenseDeactivated: ComponentStory<
  typeof SingleSignOnPage
> = () => {
  return <SingleSignOnPage />;
};
LicenseDeactivated.storyName = '💠 License Deactivated';
LicenseDeactivated.parameters = {
  msw: [eeLicenseInfo.deactivated],
  consoleType: 'pro-lite',
};
