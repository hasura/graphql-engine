import { Meta, StoryObj } from '@storybook/react';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { eeLicenseInfo } from '../mocks/http';

import { ConsoleTypeDecorator } from '../../../storybook/decorators';
import { NavbarButton as EnterpriseButton } from './NavbarButton';

export default {
  title: 'features/EETrial/NavbarButton',
  component: EnterpriseButton,
  decorators: [
    ReactQueryDecorator(),
    ConsoleTypeDecorator({ consoleType: 'pro-lite' }),
  ],
} as Meta<typeof EnterpriseButton>;

export const Loading: StoryObj<typeof EnterpriseButton> = {
  render: args => (
    <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
      <EnterpriseButton />
    </div>
  ),
};

export const NoEnterpriseLicense: StoryObj<typeof EnterpriseButton> = {
  render: args => (
    <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
      <EnterpriseButton />
    </div>
  ),

  parameters: {
    msw: [eeLicenseInfo.none],
  },
};

export const ActiveEnterpriceLicense: StoryObj<typeof EnterpriseButton> = {
  render: args => (
    <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
      <EnterpriseButton />
    </div>
  ),

  parameters: {
    msw: [eeLicenseInfo.active],
  },
};

export const ExpiredEnterpriseLicenseWithGrace: StoryObj<
  typeof EnterpriseButton
> = {
  render: args => (
    <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
      <EnterpriseButton />
    </div>
  ),

  parameters: {
    msw: [eeLicenseInfo.expiredWithoutGrace],
  },
};

export const ExpiredEnterpriseLicenseWithoutGrace: StoryObj<
  typeof EnterpriseButton
> = {
  render: args => (
    <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
      <EnterpriseButton />
    </div>
  ),
};

export const ExpiredEnterpriseLicenseAfterGrace: StoryObj<
  typeof EnterpriseButton
> = {
  render: args => (
    <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
      <EnterpriseButton />
    </div>
  ),

  parameters: {
    msw: [eeLicenseInfo.expiredAfterGrace],
  },
};

export const DeactivatedEnterpriseLicense: StoryObj<typeof EnterpriseButton> = {
  render: args => (
    <div className="w-full h-20 bg-slate-700 flex justify-center items-center">
      <EnterpriseButton />
    </div>
  ),

  parameters: {
    msw: [eeLicenseInfo.deactivated],
  },
};
