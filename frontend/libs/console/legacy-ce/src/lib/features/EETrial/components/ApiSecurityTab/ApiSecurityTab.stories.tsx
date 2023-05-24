import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import {
  registerEETrialLicenseActiveMutation,
  registerEETrialLicenseDeactivatedMutation,
  registerEETrialLicenseExpiredMutation,
} from '../../mocks/registration.mock';
import { ApiSecurityTabEELiteWrapper } from './ApiSecurityTab';
import { SecurityTabs } from '../../../../components/Services/ApiExplorer/Security/SecurityTabs';
import { eeLicenseInfo } from '../../mocks/http';
import { useQueryClient } from 'react-query';
import { EE_LICENSE_INFO_QUERY_NAME } from '../../constants';

export default {
  title: 'features / EETrial / API Security Tab 🧬️',
  component: ApiSecurityTabEELiteWrapper,
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
} as ComponentMeta<typeof ApiSecurityTabEELiteWrapper>;

export const Default: ComponentStory<
  typeof ApiSecurityTabEELiteWrapper
> = () => {
  return (
    <ApiSecurityTabEELiteWrapper>
      <SecurityTabs tabName="api_limits" />
    </ApiSecurityTabEELiteWrapper>
  );
};
Default.storyName = '💠 Default';
Default.parameters = {
  msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.none],
  consoleType: 'pro-lite',
};

export const LicenseActive: ComponentStory<
  typeof ApiSecurityTabEELiteWrapper
> = () => {
  return (
    <ApiSecurityTabEELiteWrapper>
      <SecurityTabs tabName="api_limits" />
    </ApiSecurityTabEELiteWrapper>
  );
};
LicenseActive.storyName = '💠 License Active';
LicenseActive.parameters = {
  msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.active],
  consoleType: 'pro-lite',
};

export const LicenseExpired: ComponentStory<
  typeof ApiSecurityTabEELiteWrapper
> = () => {
  return (
    <ApiSecurityTabEELiteWrapper>
      <SecurityTabs tabName="api_limits" />
    </ApiSecurityTabEELiteWrapper>
  );
};
LicenseExpired.storyName = '💠 License Expired';
LicenseExpired.parameters = {
  msw: [registerEETrialLicenseExpiredMutation, eeLicenseInfo.expired],
  consoleType: 'pro-lite',
};

export const LicenseDeactivated: ComponentStory<
  typeof ApiSecurityTabEELiteWrapper
> = () => {
  return (
    <ApiSecurityTabEELiteWrapper>
      <SecurityTabs tabName="api_limits" />
    </ApiSecurityTabEELiteWrapper>
  );
};
LicenseDeactivated.storyName = '💠 License Deactivated';
LicenseDeactivated.parameters = {
  msw: [registerEETrialLicenseDeactivatedMutation, eeLicenseInfo.deactivated],
  consoleType: 'pro-lite',
};
