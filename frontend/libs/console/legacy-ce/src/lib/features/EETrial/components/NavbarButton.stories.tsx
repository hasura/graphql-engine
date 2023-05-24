import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
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
} as ComponentMeta<typeof EnterpriseButton>;

export const Loading: ComponentStory<typeof EnterpriseButton> = args => (
  <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
    <EnterpriseButton globals={{ consoleType: 'pro-lite' } as any} />
  </div>
);
Loading.parameters = {
  consoleType: 'pro-lite',
};

export const NoEnterpriseLicense: ComponentStory<
  typeof EnterpriseButton
> = args => (
  <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
    <EnterpriseButton globals={{ consoleType: 'pro-lite' } as any} />
  </div>
);
NoEnterpriseLicense.parameters = {
  msw: [eeLicenseInfo.none],
  consoleType: 'pro-lite',
};

export const ActiveEnterpriceLicense: ComponentStory<
  typeof EnterpriseButton
> = args => (
  <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
    <EnterpriseButton globals={{ consoleType: 'pro-lite' } as any} />
  </div>
);
ActiveEnterpriceLicense.parameters = {
  msw: [eeLicenseInfo.active],
  consoleType: 'pro-lite',
};

export const ExpiredEnterpriseLicenseWithGrace: ComponentStory<
  typeof EnterpriseButton
> = args => (
  <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
    <EnterpriseButton globals={{ consoleType: 'pro-lite' } as any} />
  </div>
);
ExpiredEnterpriseLicenseWithGrace.parameters = {
  msw: [eeLicenseInfo.expired],
  consoleType: 'pro-lite',
};

export const ExpiredEnterpriseLicenseWithoutGrace: ComponentStory<
  typeof EnterpriseButton
> = args => (
  <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
    <EnterpriseButton globals={{ consoleType: 'pro-lite' } as any} />
  </div>
);
ExpiredEnterpriseLicenseWithGrace.parameters = {
  msw: [eeLicenseInfo.expiredWithoutGrace],
  consoleType: 'pro-lite',
};

export const ExpiredEnterpriseLicenseAfterGrace: ComponentStory<
  typeof EnterpriseButton
> = args => (
  <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
    <EnterpriseButton globals={{ consoleType: 'pro-lite' } as any} />
  </div>
);
ExpiredEnterpriseLicenseAfterGrace.parameters = {
  msw: [eeLicenseInfo.expiredAfterGrace],
  consoleType: 'pro-lite',
};

export const DeactivatedEnterpriseLicense: ComponentStory<
  typeof EnterpriseButton
> = args => (
  <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
    <EnterpriseButton globals={{ consoleType: 'pro-lite' } as any} />
  </div>
);
DeactivatedEnterpriseLicense.parameters = {
  msw: [eeLicenseInfo.deactivated],
  consoleType: 'pro-lite',
};
