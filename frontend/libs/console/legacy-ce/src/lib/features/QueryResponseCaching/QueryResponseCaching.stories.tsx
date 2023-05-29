import React from 'react';
import { StoryObj, Meta } from '@storybook/react';

import { QueryResponseCaching } from './QueryResponseCaching';
import {
  registerEETrialLicenseActiveMutation,
  registerEETrialLicenseAlreadyAppliedMutation,
} from '../EETrial/mocks/registration.mock';
import { eeLicenseInfo } from '../EETrial/mocks/http';
import { useQueryClient } from 'react-query';
import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { EE_LICENSE_INFO_QUERY_NAME } from '../EETrial';

export default {
  title: 'Features/Settings/Query Response Caching',
  component: QueryResponseCaching,
  parameters: {
    docs: {
      source: { type: 'code', state: 'open' },
    },
  },
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
} as Meta<typeof QueryResponseCaching>;

export const UnregisteredUser: StoryObj<typeof QueryResponseCaching> = {
  render: () => {
    return <QueryResponseCaching />;
  },

  name: '💠 Unregistered User',

  parameters: {
    msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.none],
    consoleType: 'pro-lite',
  },
};

export const LicenseActive: StoryObj<typeof QueryResponseCaching> = {
  render: () => {
    return <QueryResponseCaching />;
  },

  name: '💠 License Active',

  parameters: {
    msw: [registerEETrialLicenseAlreadyAppliedMutation, eeLicenseInfo.active],
    consoleType: 'pro-lite',
  },
};
