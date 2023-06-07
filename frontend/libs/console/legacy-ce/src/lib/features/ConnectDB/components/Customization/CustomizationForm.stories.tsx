import { CustomizationForm } from '../..';
import { SimpleForm } from '../../../../new-components/Form';
import { expect } from '@storybook/jest';
import { StoryObj, Meta } from '@storybook/react';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { screen } from '@storybook/testing-library';
import React from 'react';
import { z } from 'zod';
import { action } from '@storybook/addon-actions';

const schema = z.object({
  customization: z
    .object({
      root_fields: z.object({
        namespace: z.string(),
        prefix: z.string(),
        suffix: z.string(),
      }),
      type_names: z.object({
        prefix: z.string(),
        suffix: z.string(),
      }),
    })
    .optional(),
});

export default {
  title: 'Data/Connect/GraphQL Field Customization',
  component: CustomizationForm,
  decorators: [
    s => {
      return (
        <SimpleForm schema={schema} onSubmit={action('onSubmit')}>
          {s}
        </SimpleForm>
      );
    },
  ],
} as Meta;

export const Primary: StoryObj<typeof CustomizationForm> = {
  args: {
    defaultOpen: true,
  },

  name: '🧪 Testing - input interaction',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    inputIds.forEach(async id => {
      const parts = id.split('.');
      const subHeading = parts[1];
      const fieldName = parts[2];
      const textVal = `some ${subHeading} ${fieldName}`;

      await waitFor(async () => {
        await userEvent.type(canvas.getByTestId(id), textVal);
      });

      await waitFor(async () => {
        await expect(screen.getByTestId(id)).toHaveValue(textVal);
      });
    });
  },
};

const inputIds = [
  'customization.root_fields.namespace',
  'customization.root_fields.prefix',
  'customization.root_fields.suffix',
  'customization.type_names.prefix',
  'customization.type_names.suffix',
];
