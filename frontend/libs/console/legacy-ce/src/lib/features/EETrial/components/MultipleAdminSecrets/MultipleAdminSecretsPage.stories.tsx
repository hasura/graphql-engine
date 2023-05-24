import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { registerEETrialLicenseActiveMutation } from '../../mocks/registration.mock';
import { MultipleAdminSecretsPage } from './MultipleAdminSecretsPage';
import { eeLicenseInfo } from '../../mocks/http';
import { useQueryClient } from 'react-query';
import { EE_LICENSE_INFO_QUERY_NAME } from '../../constants';

export default {
  title: 'features / EETrial / Multiple Admin Secrets Page 🧬️',
  component: MultipleAdminSecretsPage,
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
} as ComponentMeta<typeof MultipleAdminSecretsPage>;

export const Default: ComponentStory<typeof MultipleAdminSecretsPage> = () => {
  return <MultipleAdminSecretsPage />;
};
Default.storyName = '💠 Default';
Default.parameters = {
  msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.none],
  consoleType: 'pro-lite',
};

export const LicenseActive: ComponentStory<
  typeof MultipleAdminSecretsPage
> = () => {
  return <MultipleAdminSecretsPage />;
};
LicenseActive.storyName = '💠 License Active';
LicenseActive.parameters = {
  msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.active],
  consoleType: 'pro-lite',
};

export const LicenseExpired: ComponentStory<
  typeof MultipleAdminSecretsPage
> = () => {
  return <MultipleAdminSecretsPage />;
};
LicenseExpired.storyName = '💠 License Expired';
LicenseExpired.parameters = {
  msw: [eeLicenseInfo.expired],
  consoleType: 'pro-lite',
};

export const LicenseDeactivated: ComponentStory<
  typeof MultipleAdminSecretsPage
> = () => {
  return <MultipleAdminSecretsPage />;
};
LicenseDeactivated.storyName = '💠 License Deactivated';
LicenseDeactivated.parameters = {
  msw: [eeLicenseInfo.deactivated],
  consoleType: 'pro-lite',
};
