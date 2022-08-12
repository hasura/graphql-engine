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

import { allSchemas, allFunctions } from '../mocks/mockData';
import { QueryType } from '../types';

export default {
  title: 'Features/Permissions Form/Components/Row Section',
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

const dataLeaf = {
  type: 'schema',
  name: 'users',
  leaf: {
    type: 'table',
    name: 'users',
  },
};

// this will be moved into a utils folder
const allRowChecks = [
  {
    queryType: 'insert' as QueryType,
    value: '{"id":{"_eq":1}}',
  },
  {
    queryType: 'select' as QueryType,
    value: '{"id":{"_eq":1}}',
  },
];

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
    dataLeaf,
    queryType: 'delete',
    allRowChecks,
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
    ...Insert!.args!.section!,
    queryType: 'select',
    allRowChecks,
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
    allRowChecks,
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
    allRowChecks,
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
