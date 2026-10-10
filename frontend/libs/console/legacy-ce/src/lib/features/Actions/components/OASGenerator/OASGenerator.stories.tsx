import React from 'react';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { ReactQueryDecorator, handlers } from '@hasura/shared/testing';
import { expect, userEvent, within } from 'storybook/test';
import { OASGenerator, OASGeneratorProps } from './OASGenerator';
import petstore from './fixtures/petstore.json';

const meta = {
  title: 'Features/Actions/OASGenerator',
  component: OASGenerator,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers({ delay: 500 }),
  },
  argTypes: {
    onGenerate: { action: 'Create Action' },
    onDelete: { action: 'Create Action' },
    disabled: { type: 'boolean' },
  },
} satisfies Meta<typeof OASGenerator>;

export default meta;

export const Default: StoryObj<OASGeneratorProps> = {
  render: (args) => {
    return <OASGenerator {...args} />;
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    const input = canvas.getByTestId('file');
    await userEvent.upload(
      input,
      new File([JSON.stringify(petstore)], 'test.json', {
        type: 'application/json',
      }),
    );

    // wait for two seconds
    await new Promise((resolve) => setTimeout(resolve, 2000));

    // wait for searchbox to appear
    const searchBox = await canvas.findByTestId('search');

    // count number of operations
    await expect(canvas.getAllByTestId(/^operation.*/)).toHaveLength(4);

    // search operations with 'get'
    await userEvent.type(searchBox, 'GET');
    // count filtered number of operations
    await expect(canvas.getAllByTestId(/^operation.*/)).toHaveLength(2);
    // clear search
    await userEvent.clear(searchBox);
    // search not existing operation
    await userEvent.type(searchBox, 'not-existing');
    // look for 'No endpoints found' message
    await expect(canvas.queryAllByTestId(/^operation.*/)).toHaveLength(0);
    // clear search
    await userEvent.clear(searchBox);
  },
};
