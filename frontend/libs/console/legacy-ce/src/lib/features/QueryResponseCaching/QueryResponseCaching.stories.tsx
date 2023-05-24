import React from 'react';
import { ComponentMeta, ComponentStory } from '@storybook/react';

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
} as ComponentMeta<typeof QueryResponseCaching>;

export const UnregisteredUser: ComponentStory<
  typeof QueryResponseCaching
> = () => {
  return <QueryResponseCaching />;
};
UnregisteredUser.storyName = '💠 Unregistered User';
UnregisteredUser.parameters = {
  msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.none],
  consoleType: 'pro-lite',
};

export const LicenseActive: ComponentStory<
  typeof QueryResponseCaching
> = () => {
  return <QueryResponseCaching />;
};
LicenseActive.storyName = '💠 License Active';
LicenseActive.parameters = {
  msw: [registerEETrialLicenseAlreadyAppliedMutation, eeLicenseInfo.active],
  consoleType: 'pro-lite',
};
