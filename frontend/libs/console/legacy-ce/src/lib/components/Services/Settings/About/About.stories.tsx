import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
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
} as Meta<typeof About>;

export const LoadingServerVersion: StoryObj<typeof About> = {
  render: args => (
    <div className="flex justify-center">
      <About serverVersion="" consoleAssetVersion="9acd324" />
    </div>
  ),
};

export const WithoutEnterpriseAccess: StoryObj<typeof About> = {
  render: args => (
    <div className="flex justify-center">
      <About serverVersion="v2.17.0" consoleAssetVersion="9acd324" />
    </div>
  ),
};

export const WithoutEnterpriseLicense: StoryObj<typeof About> = {
  render: args => (
    <div className="flex justify-center">
      <About serverVersion="v2.17.0" consoleAssetVersion="9acd324" />
    </div>
  ),

  parameters: {
    msw: [eeLicenseInfo.none],
    consoleType: 'pro-lite',
  },
};

export const DeactivatedEnterpriseLicense: StoryObj<typeof About> = {
  render: args => (
    <div className="flex justify-center">
      <About serverVersion="v2.17.0" consoleAssetVersion="9acd324" />
    </div>
  ),

  parameters: {
    msw: [eeLicenseInfo.deactivated],
    consoleType: 'pro-lite',
  },
};

export const ExpiredEnterpriseLicense: StoryObj<typeof About> = {
  render: args => (
    <div className="flex justify-center">
      <About serverVersion="v2.17.0" consoleAssetVersion="9acd324" />
    </div>
  ),

  parameters: {
    msw: [eeLicenseInfo.expired],
    consoleType: 'pro-lite',
  },
};

export const ActiveEnterpriseLicense: StoryObj<typeof About> = {
  render: args => (
    <div className="flex justify-center">
      <About serverVersion="v2.17.0" consoleAssetVersion="9acd324" />
    </div>
  ),

  parameters: {
    msw: [eeLicenseInfo.active],
    consoleType: 'pro-lite',
  },
};
