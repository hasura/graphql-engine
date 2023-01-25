import React from 'react';
import { Story, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { handlers } from '@/mocks/metadata.mock';
import {
  AllowListPermissions,
  AllowListPermissionsTabProps,
} from './AllowListPermissions';

export default {
  title: 'Features/Allow List/Allow List Permissions',
  component: AllowListPermissions,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers({ delay: 500 }),
  },
} as Meta;

export const Default: Story<AllowListPermissionsTabProps> = args => {
  return <AllowListPermissions {...args} />;
};

Default.args = {
  collectionName: 'allowed-queries',
};
