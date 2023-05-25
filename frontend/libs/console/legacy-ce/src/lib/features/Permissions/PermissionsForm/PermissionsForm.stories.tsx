import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';

import { PermissionsForm, PermissionsFormProps } from './PermissionsForm';
import { handlers } from './mocks/handlers.mock';

export default {
  title: 'Features/Permissions/Form',
  component: PermissionsForm,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta;

const roleName = 'user';

export const GDCSelect: StoryObj<PermissionsFormProps> = {
  args: {
    dataSourceName: 'sqlite',
    queryType: 'select',
    table: ['Artist'],
    roleName,
    handleClose: () => {},
  },
};
