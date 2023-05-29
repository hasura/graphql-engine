import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { eeLicenseInfo } from '../mocks/http';

import { NavbarButton as EnterpriseButton } from './NavbarButton';
import { useQueryClient } from 'react-query';
import { EE_LICENSE_INFO_QUERY_NAME } from '../constants';

export default {
  title: 'features/EETrial/NavbarButton',
  component: EnterpriseButton,
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
} as Meta<typeof EnterpriseButton>;

export const Loading: StoryObj<typeof EnterpriseButton> = {
  render: args => (
    <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
      <EnterpriseButton globals={{ consoleType: 'pro-lite' } as any} />
    </div>
  ),

  parameters: {
    consoleType: 'pro-lite',
  },
};

export const NoEnterpriseLicense: StoryObj<typeof EnterpriseButton> = {
  render: args => (
    <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
      <EnterpriseButton globals={{ consoleType: 'pro-lite' } as any} />
    </div>
  ),

  parameters: {
    msw: [eeLicenseInfo.none],
    consoleType: 'pro-lite',
  },
};

export const ActiveEnterpriceLicense: StoryObj<typeof EnterpriseButton> = {
  render: args => (
    <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
      <EnterpriseButton globals={{ consoleType: 'pro-lite' } as any} />
    </div>
  ),

  parameters: {
    msw: [eeLicenseInfo.active],
    consoleType: 'pro-lite',
  },
};

export const ExpiredEnterpriseLicenseWithGrace: StoryObj<
  typeof EnterpriseButton
> = {
  render: args => (
    <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
      <EnterpriseButton globals={{ consoleType: 'pro-lite' } as any} />
    </div>
  ),

  parameters: {
    msw: [eeLicenseInfo.expiredWithoutGrace],
    consoleType: 'pro-lite',
  },
};

export const ExpiredEnterpriseLicenseWithoutGrace: StoryObj<
  typeof EnterpriseButton
> = {
  render: args => (
    <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
      <EnterpriseButton globals={{ consoleType: 'pro-lite' } as any} />
    </div>
  ),
};

export const ExpiredEnterpriseLicenseAfterGrace: StoryObj<
  typeof EnterpriseButton
> = {
  render: args => (
    <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
      <EnterpriseButton globals={{ consoleType: 'pro-lite' } as any} />
    </div>
  ),

  parameters: {
    msw: [eeLicenseInfo.expiredAfterGrace],
    consoleType: 'pro-lite',
  },
};

export const DeactivatedEnterpriseLicense: StoryObj<typeof EnterpriseButton> = {
  render: args => (
    <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
      <EnterpriseButton globals={{ consoleType: 'pro-lite' } as any} />
    </div>
  ),

  parameters: {
    msw: [eeLicenseInfo.deactivated],
    consoleType: 'pro-lite',
  },
};
