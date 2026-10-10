import { Meta, StoryObj } from '@storybook/react-webpack5';

import {
  ConsoleTypeDecorator,
  ReactQueryDecorator,
} from '@hasura/shared/testing';
import { eeLicenseInfo } from '../EETrial/mocks/http';
import {
  registerEETrialLicenseActiveMutation,
  registerEETrialLicenseAlreadyAppliedMutation,
} from '../EETrial/mocks/registration.mock';
import { QueryResponseCaching } from './QueryResponseCaching';

export default {
  title: 'Features/Settings/Query Response Caching',
  component: QueryResponseCaching,
  parameters: {
    docs: {
      source: { type: 'code', state: 'open' },
    },
  },
  decorators: [
    ReactQueryDecorator(),
    ConsoleTypeDecorator({ consoleType: 'pro-lite' }),
  ],
} as Meta<typeof QueryResponseCaching>;

export const UnregisteredUser: StoryObj<typeof QueryResponseCaching> = {
  render: () => {
    return <QueryResponseCaching />;
  },

  name: '💠 Unregistered User',

  parameters: {
    msw: [registerEETrialLicenseActiveMutation, eeLicenseInfo.none],
  },
};

export const LicenseActive: StoryObj<typeof QueryResponseCaching> = {
  render: () => {
    return <QueryResponseCaching />;
  },

  name: '💠 License Active',

  parameters: {
    msw: [registerEETrialLicenseAlreadyAppliedMutation, eeLicenseInfo.active],
  },
};
