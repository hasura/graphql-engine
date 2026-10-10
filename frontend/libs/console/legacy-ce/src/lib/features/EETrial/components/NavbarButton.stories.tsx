import { Meta, StoryObj } from '@storybook/react-webpack5';
import { Flex } from '@radix-ui/themes';
import {
  ConsoleTypeDecorator,
  ReactQueryDecorator,
} from '@hasura/shared/testing';
import { eeLicenseInfo } from '../mocks/http';

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
  render: (args) => (
    <Flex justify="center" align="center" className="w-full h-20 bg-slate-700">
      <EnterpriseButton />
    </Flex>
  ),
};

export const NoEnterpriseLicense: StoryObj<typeof EnterpriseButton> = {
  render: (args) => (
    <Flex justify="center" align="center" className="w-full h-20 bg-slate-700">
      <EnterpriseButton />
    </Flex>
  ),

  parameters: {
    msw: [eeLicenseInfo.none],
  },
};

export const ActiveEnterpriceLicense: StoryObj<typeof EnterpriseButton> = {
  render: (args) => (
    <Flex justify="center" align="center" className="w-full h-20 bg-slate-700">
      <EnterpriseButton />
    </Flex>
  ),

  parameters: {
    msw: [eeLicenseInfo.active],
  },
};

export const ExpiredEnterpriseLicenseWithGrace: StoryObj<
  typeof EnterpriseButton
> = {
  render: (args) => (
    <Flex justify="center" align="center" className="w-full h-20 bg-slate-700">
      <EnterpriseButton />
    </Flex>
  ),

  parameters: {
    msw: [eeLicenseInfo.expiredWithoutGrace],
  },
};

export const ExpiredEnterpriseLicenseWithoutGrace: StoryObj<
  typeof EnterpriseButton
> = {
  render: (args) => (
    <Flex justify="center" align="center" className="w-full h-20 bg-slate-700">
      <EnterpriseButton />
    </Flex>
  ),
};

export const ExpiredEnterpriseLicenseAfterGrace: StoryObj<
  typeof EnterpriseButton
> = {
  render: (args) => (
    <Flex justify="center" align="center" className="w-full h-20 bg-slate-700">
      <EnterpriseButton />
    </Flex>
  ),

  parameters: {
    msw: [eeLicenseInfo.expiredAfterGrace],
  },
};

export const DeactivatedEnterpriseLicense: StoryObj<typeof EnterpriseButton> = {
  render: (args) => (
    <Flex justify="center" align="center" className="w-full h-20 bg-slate-700">
      <EnterpriseButton />
    </Flex>
  ),

  parameters: {
    msw: [eeLicenseInfo.deactivated],
  },
};
