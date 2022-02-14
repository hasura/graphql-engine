import React from 'react';
import { Story, Meta } from '@storybook/react';
import { Form } from '@/new-components/Form';
import { z } from 'zod';

import {
  RowPermissionsSection,
  RowPermissionsProps,
  RowPermissionsSectionWrapper,
  RowPermissionsWrapperProps,
} from './RowPermissions';

import { currentSchema, allSchemas, allFunctions } from '../mocks/mockData';
import { QueryType } from '../types';

export default {
  title: 'Permissions Form/Components/Row Section',
  component: RowPermissionsSection,
  decorators: [
    (StoryComponent: React.FC) => (
      <Form schema={z.any()} onSubmit={() => {}}>
        {() => <StoryComponent />}
      </Form>
    ),
  ],
  parameters: { chromatic: { disableSnapshot: true } },
} as Meta;

const roleName = 'two';

// this will be moved into a utils folder
const allRowChecks = ({ role, query }: { role: string; query: string }) => {
  const currentRole = currentSchema.permissions.find(
    ({ role_name }) => role === role_name
  );

  if (currentRole) {
    const { permissions } = currentRole;
    return Object.entries(permissions)
      .filter(([name, info]) => name !== query && info.filter)
      .map(([name, info]) => ({
        queryType: name as QueryType,
        filter: JSON.stringify(info.filter),
      }));
  }

  return [];
};

interface Props {
  wrapper: RowPermissionsWrapperProps;
  section: RowPermissionsProps;
}

export const Insert: Story<Props> = args => (
  <RowPermissionsSectionWrapper {...args.wrapper}>
    <RowPermissionsSection {...args.section} />
  </RowPermissionsSectionWrapper>
);
Insert.args = {
  wrapper: { roleName, queryType: 'insert', defaultOpen: true },
  section: {
    schemaName: 'public',
    tableName: 'users',
    queryType: 'delete',
    allRowChecks: allRowChecks({ role: roleName, query: 'insert' }),
    allSchemas,
    allFunctions,
  },
};

export const Select: Story<Props> = args => (
  <RowPermissionsSectionWrapper {...args.wrapper}>
    <RowPermissionsSection {...args.section} />
  </RowPermissionsSectionWrapper>
);
Select.args = {
  wrapper: { roleName, queryType: 'select', defaultOpen: true },
  section: {
    ...Insert.args.section!,
    queryType: 'select',
    allRowChecks: allRowChecks({ role: roleName, query: 'select' }),
  },
};

export const Update: Story<Props> = args => (
  <RowPermissionsSectionWrapper {...args.wrapper}>
    <RowPermissionsSection {...args.section} />
  </RowPermissionsSectionWrapper>
);
Update.args = {
  wrapper: { roleName, queryType: 'update', defaultOpen: true },
  section: {
    ...Insert.args.section!,
    queryType: 'update',
    allRowChecks: allRowChecks({ role: roleName, query: 'update' }),
  },
};

export const Delete: Story<Props> = args => (
  <RowPermissionsSectionWrapper {...args.wrapper}>
    <RowPermissionsSection {...args.section} />
  </RowPermissionsSectionWrapper>
);
Delete.args = {
  wrapper: { roleName, queryType: 'delete', defaultOpen: true },
  section: {
    ...Insert.args.section!,
    queryType: 'delete',
    allRowChecks: allRowChecks({ role: roleName, query: 'delete' }),
  },
};

type ShowcaseProps = Record<string, Props>;

export const Showcase: Story<ShowcaseProps> = args => (
  <>
    <Insert {...args.insert} />
    <Select {...args.select} />
    <Update {...args.update} />
    <Delete {...args.delete} />
  </>
);
Showcase.args = {
  insert: Insert.args,
  select: Select.args,
  update: Update.args,
  delete: Delete.args,
} as ShowcaseProps;
Showcase.parameters = { chromatic: { disableSnapshot: false } };
