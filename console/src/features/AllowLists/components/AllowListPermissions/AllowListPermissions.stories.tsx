import React from 'react';
import { Story, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import {
  AllowListPermissions,
  AllowListPermissionsTabProps,
} from './AllowListPermissions';
import { handlers } from '../../hooks/AllowListPermissions/mock/handlers.mocks';

export default {
  title: 'Features/Allow List Permission/CardedTable',
  component: AllowListPermissions,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta;

export const Default: Story<AllowListPermissionsTabProps> = args => {
  return <AllowListPermissions {...args} />;
};

Default.args = {
  collectionName: 'allowed-queries',
};
