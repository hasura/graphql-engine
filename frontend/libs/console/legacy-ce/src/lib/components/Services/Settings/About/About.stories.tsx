import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { eeLicenseInfo } from '../../../../features/EETrial/mocks/http';

import { About } from './About';

export default {
  title: 'components/Services/Settings/About',
  parameters: {
    Benefits: {
      source: { type: 'code' },
    },
  },
  component: About,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof About>;

export const LoadingServerVersion: ComponentStory<typeof About> = args => (
  <div className="flex justify-center">
    <About serverVersion="" consoleAssetVersion="9acd324" />
  </div>
);

export const WithoutEnterpriseAccess: ComponentStory<typeof About> = args => (
  <div className="flex justify-center">
    <About serverVersion="v2.17.0" consoleAssetVersion="9acd324" />
  </div>
);

export const WithoutEnterpriseLicense: ComponentStory<typeof About> = args => (
  <div className="flex justify-center">
    <About serverVersion="v2.17.0" consoleAssetVersion="9acd324" />
  </div>
);
WithoutEnterpriseLicense.parameters = {
  msw: [eeLicenseInfo.none],
  consoleType: 'pro-lite',
};

export const DeactivatedEnterpriseLicense: ComponentStory<
  typeof About
> = args => (
  <div className="flex justify-center">
    <About serverVersion="v2.17.0" consoleAssetVersion="9acd324" />
  </div>
);
DeactivatedEnterpriseLicense.parameters = {
  msw: [eeLicenseInfo.deactivated],
  consoleType: 'pro-lite',
};

export const ExpiredEnterpriseLicense: ComponentStory<typeof About> = args => (
  <div className="flex justify-center">
    <About serverVersion="v2.17.0" consoleAssetVersion="9acd324" />
  </div>
);
ExpiredEnterpriseLicense.parameters = {
  msw: [eeLicenseInfo.expired],
  consoleType: 'pro-lite',
};

export const ActiveEnterpriseLicense: ComponentStory<typeof About> = args => (
  <div className="flex justify-center">
    <About serverVersion="v2.17.0" consoleAssetVersion="9acd324" />
  </div>
);
ActiveEnterpriseLicense.parameters = {
  msw: [eeLicenseInfo.active],
  consoleType: 'pro-lite',
};
