import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { z } from 'zod';
import { SimpleForm } from '../../../../new-components/Form';

import {
  ColumnPresetsSection,
  ColumnPresetsSectionProps,
} from './ColumnPresets';

export default {
  component: ColumnPresetsSection,
  decorators: [
    (StoryComponent: React.FC) => (
      <SimpleForm
        schema={z.any()}
        options={{
          defaultValues: {
            presets: [],
            rowPermissionsCheckType: 'custom',
          },
        }}
        onSubmit={() => {}}
        className="p-4"
      >
        <StoryComponent />
      </SimpleForm>
    ),
  ],
  parameters: { chromatic: { disableSnapshot: true } },
} as Meta;

const columns = ['id', 'name', 'description'];

export const Insert: StoryObj<ColumnPresetsSectionProps> = {
  render: args => <ColumnPresetsSection {...args} />,
  args: {
    queryType: 'insert',
    columns,
  },
};

export const Update: StoryObj<ColumnPresetsSectionProps> = {
  render: args => <ColumnPresetsSection {...args} />,
  args: {
    ...Insert.args,
    queryType: 'update',
  },
};

export const WithPartialPresets: StoryObj<ColumnPresetsSectionProps> = {
  render: args => <ColumnPresetsSection {...args} />,
  args: {
    ...Insert.args,
  },

  decorators: [
    (S: React.FC) => (
      <SimpleForm
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
        className="p-4"
      >
        <S />
      </SimpleForm>
    ),
  ],
};

export const WithAllPresets: StoryObj<ColumnPresetsSectionProps> = {
  render: args => <ColumnPresetsSection {...args} />,
  args: {
    ...Insert.args,
  },

  decorators: [
    (S: React.FC) => (
      <SimpleForm
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
        className="p-4"
      >
        <S />
      </SimpleForm>
    ),
  ],
};

type ShowcaseProps = {
  insert: ColumnPresetsSectionProps;
  update: ColumnPresetsSectionProps;
  partialPresets: ColumnPresetsSectionProps;
  allPresets: ColumnPresetsSectionProps;
};

export const Showcase: StoryObj<ShowcaseProps> = {
  render: args => (
    <>
      {Object.entries(args).map(([, value]) => (
        <ColumnPresetsSection {...value} />
      ))}
    </>
  ),
  args: {
    insert: Insert.args,
    update: Update.args,
    partialPresets: WithPartialPresets.args,
    allPresets: WithAllPresets.args,
  } as ShowcaseProps,

  parameters: { chromatic: { disableSnapshot: false } },
};
