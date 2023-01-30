import React from 'react';
import { Story, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { handlers } from '@/mocks/metadata.mock';
import { within, userEvent } from '@storybook/testing-library';
import { waitFor } from '@testing-library/react';
import { expect } from '@storybook/jest';
import { OasGeneratorModal, OasGeneratorModalProps } from './OASGeneratorModal';
import petstore from './petstore.json';

export default {
  title: 'Features/Actions/OASGeneratorModal',
  component: OasGeneratorModal,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers({ delay: 500 }),
  },
} as Meta;

export const Default: Story<OasGeneratorModalProps> = args => {
  return <OasGeneratorModal {...args} />;
};

Default.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  const input = canvas.getByTestId('file');
  userEvent.upload(
    input,
    new File([JSON.stringify(petstore)], 'test.json', {
      type: 'application/json',
    })
  );

  await waitFor(() => {
    return canvas.queryByTestId('search');
  });

  // wait for searchbox to appear
  const searchBox = await canvas.findByTestId('search');
  // count number of operations
  expect(canvas.getAllByTestId(/^operation.*/)).toHaveLength(4);
  // search operations with 'get'
  userEvent.type(searchBox, 'GET');
  // count filtered number of operations
  expect(canvas.getAllByTestId(/^operation.*/)).toHaveLength(2);
  // clear search
  userEvent.clear(searchBox);
  // search not existing operation
  userEvent.type(searchBox, 'not-existing');
  // look for 'No endpoints found' message
  expect(canvas.getByText(/No endpoints found/)).toBeInTheDocument();
  // clear search
  userEvent.clear(searchBox);
  // click on 'POST' badge
  userEvent.click(canvas.getByTestId('badge-POST'));
  // count filtered number of operations
  expect(canvas.getAllByTestId(/^operation.*/)).toHaveLength(1);
  // Generate action button should be disabled
  expect(canvas.getByText('Generate Action').parentElement).toBeDisabled();
  // click on the first operation
  userEvent.click(canvas.getByTestId(/^operation.*/));
  // wait for generate action button to be enabled
  await waitFor(() => {
    return expect(
      canvas.getByText('Generate Action').parentElement
    ).toHaveAttribute('disabled', '');
  });
};
