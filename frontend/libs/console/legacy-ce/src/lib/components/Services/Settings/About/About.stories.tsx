import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { eeLicenseInfo } from '../../../../features/EETrial/mocks/http';

import { About } from './About';
import { ConsoleTypeDecorator } from '../../../../storybook/decorators';

export default {
  title: 'components/Services/Settings/About',
  parameters: {
    Benefits: {
      source: { type: 'code' },
    },
    mockdate: new Date('2020-01-14T15:47:18.502Z'),
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
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro-lite' })],
  parameters: {
    msw: [eeLicenseInfo.none],
  },
};

export const DeactivatedEnterpriseLicense: StoryObj<typeof About> = {
  render: args => (
    <div className="flex justify-center">
      <About serverVersion="v2.17.0" consoleAssetVersion="9acd324" />
    </div>
  ),
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro-lite' })],
  parameters: {
    msw: [eeLicenseInfo.deactivated],
  },
};

export const ExpiredEnterpriseLicense: StoryObj<typeof About> = {
  render: args => (
    <div className="flex justify-center">
      <About serverVersion="v2.17.0" consoleAssetVersion="9acd324" />
    </div>
  ),
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro-lite' })],
  parameters: {
    msw: [eeLicenseInfo.expired],
  },
};

export const ActiveEnterpriseLicense: StoryObj<typeof About> = {
  render: args => (
    <div className="flex justify-center">
      <About serverVersion="v2.17.0" consoleAssetVersion="9acd324" />
    </div>
  ),
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro-lite' })],
  parameters: {
    msw: [eeLicenseInfo.active],
  },
};
