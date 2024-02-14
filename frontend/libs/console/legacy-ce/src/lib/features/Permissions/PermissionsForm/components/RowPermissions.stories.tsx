/* eslint-disable react/jsx-pascal-case */
import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
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

const defaultProps: Props = {
  wrapper: { roleName, queryType: 'insert', defaultOpen: true },

  section: {
    table: {
      schema: 'public',
      name: 'user',
    },
    dataSourceName: 'chinook',
    queryType: 'delete',
    supportedOperators: [],
    defaultValues: undefined,
    permissionsKey: 'check',
    roleName: '',
  },
};

export const Insert: StoryObj<Props> = {
  render: args => (
    <RowPermissionsSectionWrapper {...args.wrapper}>
      <RowPermissionsSection {...args.section} />
    </RowPermissionsSectionWrapper>
  ),

  args: {
    ...defaultProps,
  },
};

export const Select: StoryObj<Props> = {
  render: args => (
    <RowPermissionsSectionWrapper {...args.wrapper}>
      <RowPermissionsSection {...args.section} />
    </RowPermissionsSectionWrapper>
  ),

  args: {
    wrapper: { roleName, queryType: 'select', defaultOpen: true },
    section: {
      ...defaultProps.section,
      queryType: 'select',
    },
  },
};

export const Update: StoryObj<Props> = {
  render: args => (
    <RowPermissionsSectionWrapper {...args.wrapper}>
      <RowPermissionsSection {...args.section} />
    </RowPermissionsSectionWrapper>
  ),

  args: {
    wrapper: { roleName, queryType: 'update', defaultOpen: true },
    section: {
      ...defaultProps.section,
      queryType: 'update',
    },
  },
};

export const Delete: StoryObj<Props> = {
  render: args => (
    <RowPermissionsSectionWrapper {...args.wrapper}>
      <RowPermissionsSection {...args.section} />
    </RowPermissionsSectionWrapper>
  ),

  args: {
    wrapper: { roleName, queryType: 'delete', defaultOpen: true },
    section: {
      ...defaultProps.section,
      queryType: 'delete',
    },
  },
};

type ShowcaseProps = {
  insert: Props;
  select: Props;
  update: Props;
  delete: Props;
};

export const Showcase: StoryObj<ShowcaseProps> = {
  render: args => (
    <>
      {Object.entries(args).map(([, value]) => (
        <RowPermissionsSectionWrapper {...value.wrapper}>
          <RowPermissionsSection {...value.section} />
        </RowPermissionsSectionWrapper>
      ))}
    </>
  ),

  args: {
    insert: Insert.args,
    select: Select.args,
    update: Update.args,
    delete: Delete.args,
  } as ShowcaseProps,
  parameters: { chromatic: { disableSnapshot: false } },
};
