import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';

import { PermissionsForm, PermissionsFormProps } from './PermissionsForm';
import { handlers } from './mocks/handlers.mock';

export default {
  component: PermissionsForm,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta;

const roleName = 'user';

export const GDCSelect: StoryObj<PermissionsFormProps> = {
  args: {
    dataSourceName: 'Lite',
    queryType: 'select',
    table: ['Artist'],
    roleName,
    handleClose: () => {},
  },
};

export const GDCInsert: StoryObj<PermissionsFormProps> = {
  args: {
    dataSourceName: 'Lite',
    queryType: 'insert',
    table: ['Artist'],
    roleName,
    handleClose: () => {},
  },
};

export const GDCUpdate: StoryObj<PermissionsFormProps> = {
  args: {
    dataSourceName: 'Lite',
    queryType: 'update',
    table: ['Artist'],
    roleName,
    handleClose: () => {},
  },
};
