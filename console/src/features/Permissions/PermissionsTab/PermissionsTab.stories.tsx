import React from 'react';

import { Story, Meta } from '@storybook/react';

import { ReactQueryDecorator } from '@/storybook/decorators/react-query';

import { PermissionsTab, PermissionsTabProps } from './PermissionsTab';
import { handlers } from '../PermissionsForm/mocks/handlers.mock';

export default {
  title: 'Features/Permissions',
  component: PermissionsTab,
  decorators: [ReactQueryDecorator()],
} as Meta;

export const GDC: Story<PermissionsTabProps> = args => (
  <PermissionsTab {...args} />
);
GDC.args = {
  dataSourceName: 'sqlite',
  table: ['Artist'],
};

GDC.parameters = {
  msw: handlers(),
};

export const GDCNoMocks: Story<PermissionsTabProps> = args => (
  <PermissionsTab {...args} />
);
GDCNoMocks.args = {
  dataSourceName: 'sqlite',
  table: ['Artist'],
};
