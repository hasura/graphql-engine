import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
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
} as Meta<typeof ApiSecurityTabEELiteWrapper>;

export const Default: StoryObj<typeof ApiSecurityTabEELiteWrapper> = {
  render: () => {
    return (
      <ApiSecurityTabEELiteWrapper>
        <SecurityTabs tabName="api_limits" />
      </ApiSecurityTabEELiteWrapper>
    );
  },

  name: '💠 Default',

  parameters: {
    msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.none],
    consoleType: 'pro-lite',
  },
};

export const LicenseActive: StoryObj<typeof ApiSecurityTabEELiteWrapper> = {
  render: () => {
    return (
      <ApiSecurityTabEELiteWrapper>
        <SecurityTabs tabName="api_limits" />
      </ApiSecurityTabEELiteWrapper>
    );
  },

  name: '💠 License Active',

  parameters: {
    msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.active],
    consoleType: 'pro-lite',
  },
};

export const LicenseExpired: StoryObj<typeof ApiSecurityTabEELiteWrapper> = {
  render: () => {
    return (
      <ApiSecurityTabEELiteWrapper>
        <SecurityTabs tabName="api_limits" />
      </ApiSecurityTabEELiteWrapper>
    );
  },

  name: '💠 License Expired',

  parameters: {
    msw: [registerEETrialLicenseExpiredMutation, eeLicenseInfo.expired],
    consoleType: 'pro-lite',
  },
};

export const LicenseDeactivated: StoryObj<typeof ApiSecurityTabEELiteWrapper> =
  {
    render: () => {
      return (
        <ApiSecurityTabEELiteWrapper>
          <SecurityTabs tabName="api_limits" />
        </ApiSecurityTabEELiteWrapper>
      );
    },

    name: '💠 License Deactivated',

    parameters: {
      msw: [
        registerEETrialLicenseDeactivatedMutation,
        eeLicenseInfo.deactivated,
      ],
      consoleType: 'pro-lite',
    },
  };
