import React from 'react';
import { z } from 'zod';
import { ComponentStory, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { UpdatedForm } from '@/new-components/Form';

import { RowPermissionBuilder } from './RowPermissionBuilder';
import { createDefaultValues } from './utils';
import {
  handlers,
  schema,
  simpleExample,
  exampleWithBoolOperator,
  exampleWithRelationship,
  complicatedExample,
} from './mocks';

export default {
  title:
    'Features/Permissions Tab/Permissions Form/Components/Row Permissions Builder',
  component: RowPermissionBuilder,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta;

export const Primary: ComponentStory<typeof RowPermissionBuilder> = args => (
  <RowPermissionBuilder {...args} />
);
Primary.args = {
  tableName: 'user',
  nesting: ['filter'],
};
Primary.decorators = [
  Component => {
    return (
      <div style={{ width: 800 }}>
        <UpdatedForm schema={z.any()} onSubmit={console.log}>
          {() => {
            return <Component />;
          }}
        </UpdatedForm>
      </div>
    );
  },
];

export const WithDefaults: ComponentStory<typeof RowPermissionBuilder> =
  args => <RowPermissionBuilder {...args} />;
WithDefaults.args = {
  tableName: 'Album',
  nesting: ['filter'],
};
WithDefaults.decorators = [
  Component => {
    return (
      <div style={{ width: 800 }}>
        <UpdatedForm
          schema={z.any()}
          options={{
            defaultValues: createDefaultValues({
              tableName: 'Album',
              schema,
              existingPermission: simpleExample,
              tableConfig: {},
            }),
          }}
          onSubmit={console.log}
        >
          {() => {
            return <Component />;
          }}
        </UpdatedForm>
      </div>
    );
  },
];

export const WithDefaultsBool: ComponentStory<typeof RowPermissionBuilder> =
  args => <RowPermissionBuilder {...args} />;

WithDefaultsBool.args = {
  tableName: 'user',
  nesting: ['filter'],
};

WithDefaultsBool.decorators = [
  Component => {
    return (
      <div style={{ width: 800 }}>
        <UpdatedForm
          schema={z.any()}
          options={{
            defaultValues: createDefaultValues({
              tableName: 'user',
              schema,
              existingPermission: exampleWithBoolOperator,
              tableConfig: {},
            }),
          }}
          onSubmit={console.log}
        >
          {() => {
            return <Component />;
          }}
        </UpdatedForm>
      </div>
    );
  },
];

export const WithDefaultsRelationship: ComponentStory<
  typeof RowPermissionBuilder
> = args => <RowPermissionBuilder {...args} />;

WithDefaultsRelationship.args = {
  tableName: 'user',
  nesting: ['filter'],
};
WithDefaultsRelationship.decorators = [
  Component => {
    return (
      <div style={{ width: 800 }}>
        <UpdatedForm
          schema={z.any()}
          options={{
            defaultValues: createDefaultValues({
              tableName: 'user',
              schema,
              existingPermission: exampleWithRelationship,
              tableConfig: {},
            }),
          }}
          onSubmit={console.log}
        >
          {() => {
            return <Component />;
          }}
        </UpdatedForm>
      </div>
    );
  },
];

export const WithPointlesslyComplicatedRelationship: ComponentStory<
  typeof RowPermissionBuilder
> = args => <RowPermissionBuilder {...args} />;

WithPointlesslyComplicatedRelationship.args = {
  tableName: 'user',
  nesting: ['filter'],
};

WithPointlesslyComplicatedRelationship.decorators = [
  Component => {
    return (
      <div style={{ width: 800 }}>
        <UpdatedForm
          schema={z.any()}
          options={{
            defaultValues: createDefaultValues({
              tableName: 'user',
              schema,
              existingPermission: complicatedExample,
              tableConfig: {},
            }),
          }}
          onSubmit={console.log}
        >
          {() => {
            return <Component />;
          }}
        </UpdatedForm>
      </div>
    );
  },
];
