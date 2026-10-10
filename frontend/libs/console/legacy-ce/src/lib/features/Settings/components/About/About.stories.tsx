import { StoryObj, Meta } from '@storybook/react-webpack5';
import { Flex } from '@radix-ui/themes';
import {
  ReactQueryDecorator,
  ConsoleTypeDecorator,
} from '@hasura/shared/testing';
import { eeLicenseInfo } from '../../../EETrial/mocks/http';

import { About } from './About';

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
  render: (args) => (
    <Flex justify="center">
      <About />
    </Flex>
  ),
};

export const WithoutEnterpriseAccess: StoryObj<typeof About> = {
  render: (args) => (
    <Flex justify="center">
      <About />
    </Flex>
  ),
};

export const WithoutEnterpriseLicense: StoryObj<typeof About> = {
  render: (args) => (
    <Flex justify="center">
      <About />
    </Flex>
  ),
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro-lite' })],
  parameters: {
    msw: [eeLicenseInfo.none],
  },
};

export const DeactivatedEnterpriseLicense: StoryObj<typeof About> = {
  render: (args) => (
    <Flex justify="center">
      <About />
    </Flex>
  ),
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro-lite' })],
  parameters: {
    msw: [eeLicenseInfo.deactivated],
  },
};

export const ExpiredEnterpriseLicense: StoryObj<typeof About> = {
  render: (args) => (
    <Flex justify="center">
      <About />
    </Flex>
  ),
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro-lite' })],
  parameters: {
    msw: [eeLicenseInfo.expired],
  },
};

export const ActiveEnterpriseLicense: StoryObj<typeof About> = {
  render: (args) => (
    <Flex justify="center">
      <About />
    </Flex>
  ),
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro-lite' })],
  parameters: {
    msw: [eeLicenseInfo.active],
  },
};
