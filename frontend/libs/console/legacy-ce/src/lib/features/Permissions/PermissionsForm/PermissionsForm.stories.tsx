import { StoryObj, Meta } from '@storybook/react-webpack5';
import { ReactQueryDecorator } from '@hasura/shared/testing';

import { PermissionsForm, PermissionsFormProps } from './PermissionsForm';
import { handlers } from './mocks/handlers.mock';
import { MetadataTable, Source } from '@hasura/shared/types';

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
    source: {
      name: 'Lite',
      kind: 'sqlite',
    } as Source,
    queryType: 'select',
    table: {
      table: ['Artist'],
    } as MetadataTable,
    roleName,
    handleClose: () => {},
  },
};

export const GDCInsert: StoryObj<PermissionsFormProps> = {
  args: {
    source: {
      name: 'Lite',
      kind: 'sqlite',
    } as Source,
    queryType: 'insert',
    table: {
      table: ['Artist'],
    } as MetadataTable,
    roleName,
    handleClose: () => {},
  },
};

export const GDCUpdate: StoryObj<PermissionsFormProps> = {
  args: {
    source: {
      name: 'Lite',
      kind: 'sqlite',
    } as Source,
    queryType: 'update',
    table: {
      table: ['Artist'],
    } as MetadataTable,
    roleName,
    handleClose: () => {},
  },
};
