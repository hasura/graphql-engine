import React from 'react';
import { Meta, Story } from '@storybook/react';
import { SimpleForm } from '../../../../new-components/Form';
import { z } from 'zod';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import {
  RowPermissionsProps,
  RowPermissionsSection,
  RowPermissionsSectionWrapper,
  RowPermissionsWrapperProps,
} from './RowPermissions';

export default {
  title: 'Features/Permissions/Form/Row Section',
  component: RowPermissionsSection,
  decorators: [
    (StoryComponent: React.FC) => (
      <SimpleForm schema={z.any()} onSubmit={() => {}}>
        <StoryComponent />
      </SimpleForm>
    ),
    ReactQueryDecorator(),
  ],
  parameters: { chromatic: { disableSnapshot: true } },
} as Meta;

const roleName = 'two';

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
    table: {
      schema: 'public',
      name: 'user',
    },
    dataSourceName: 'chinook',
    queryType: 'delete',
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
    ...(Insert?.args?.section || {}),
    queryType: 'update',
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
    ...(Insert?.args?.section || {}),
    queryType: 'delete',
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
