import React from 'react';
import { Story, Meta } from '@storybook/react';
import { z } from 'zod';
import { Form } from '@/new-components/Form';

import {
  ColumnPresetsSection,
  ColumnPresetsSectionProps,
} from './ColumnPresets';

export default {
  title: 'Permissions Form/Components/Presets Section',
  component: ColumnPresetsSection,
  decorators: [
    (StoryComponent: React.FC) => (
      <Form
        schema={z.any()}
        options={{
          defaultValues: {
            presets: [],
            rowPermissionsCheckType: 'custom',
          },
        }}
        onSubmit={() => {}}
      >
        {() => <StoryComponent />}
      </Form>
    ),
  ],
  parameters: { chromatic: { disableSnapshot: true } },
} as Meta;

const columns = ['id', 'name', 'description'];

export const Insert: Story<ColumnPresetsSectionProps> = args => (
  <ColumnPresetsSection {...args} />
);
Insert.args = {
  queryType: 'insert',
  columns,
  defaultOpen: true,
};

export const Update: Story<ColumnPresetsSectionProps> = args => (
  <ColumnPresetsSection {...args} />
);
Update.args = {
  ...Insert.args,
  queryType: 'update',
};

export const WithPartialPresets: Story<ColumnPresetsSectionProps> = args => (
  <ColumnPresetsSection {...args} />
);
WithPartialPresets.args = {
  ...Insert.args,
};
WithPartialPresets.decorators = [
  (S: React.FC) => (
    <Form
      schema={z.object({
        presets: z.any(),
        rowPermissionsCheckType: z.string(),
      })}
      options={{
        defaultValues: {
          presets: [
            {
              id: 1,
              columnName: 'name',
              presetType: 'static',
              columnValue: 'Jeremy',
            },
          ],
          rowPermissionsCheckType: 'custom',
        },
      }}
      onSubmit={() => {}}
    >
      {() => <S />}
    </Form>
  ),
];

export const WithAllPresets: Story<ColumnPresetsSectionProps> = args => (
  <ColumnPresetsSection {...args} />
);
WithAllPresets.args = {
  ...Insert.args,
};
WithAllPresets.decorators = [
  (S: React.FC) => (
    <Form
      schema={z.object({
        presets: z.array(
          z.object({
            id: z.number(),
            columnName: z.string(),
            presetType: z.string(),
            columnValue: z.union([z.string(), z.number()]),
          })
        ),
        rowPermissionsCheckType: z.string(),
      })}
      options={{
        defaultValues: {
          presets: [
            {
              id: 1,
              columnName: 'id',
              presetType: 'static',
              columnValue: 1,
            },
            {
              id: 2,
              columnName: 'name',
              presetType: 'static',
              columnValue: 'Jeremy',
            },
            {
              id: 3,
              columnName: 'description',
              presetType: 'static',
              columnValue: 'A fine chap',
            },
          ],
          rowPermissionsCheckType: 'custom',
        },
      }}
      onSubmit={() => {}}
    >
      {() => <S />}
    </Form>
  ),
];

type ShowcaseProps = Record<string, ColumnPresetsSectionProps>;

export const Showcase: Story<ShowcaseProps> = args => (
  <>
    <Insert {...args.insert} />
    <Update {...args.update} />
    <WithPartialPresets {...args.partialPresets} />
    <WithAllPresets {...args.allPresets} />
  </>
);
Showcase.args = {
  insert: Insert.args,
  update: Update.args,
  partialPresets: WithPartialPresets.args,
  allPresets: WithAllPresets.args,
} as ShowcaseProps;
Showcase.parameters = { chromatic: { disableSnapshot: false } };
