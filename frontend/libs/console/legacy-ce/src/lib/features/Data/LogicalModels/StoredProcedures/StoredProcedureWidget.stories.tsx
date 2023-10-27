import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { StoredProcedureWidget } from './StoredProcedureWidget';
import { handlers } from '../LogicalModelWidget/mocks/handlers';
import {
  fireEvent,
  userEvent,
  waitFor,
  within,
} from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import {
  STORED_PROCEDURE_TRACK_ERROR,
  STORED_PROCEDURE_TRACK_SUCCESS,
} from '../constants';

export default {
  component: StoredProcedureWidget,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof StoredProcedureWidget>;

export const Basic: StoryObj<typeof StoredProcedureWidget> = {
  render: args => {
    return <StoredProcedureWidget />;
  },

  parameters: {
    msw: handlers['200'],
  },
};

export const BasicUserFlow: StoryObj<typeof StoredProcedureWidget> = {
  render: args => {
    return <StoredProcedureWidget />;
  },

  name: '🧪 Happy path test',

  parameters: {
    msw: handlers['200'],
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.selectOptions(
      await canvas.findByLabelText('Select a source', {}, { timeout: 4000 }),
      'bikes'
    );

    await userEvent.selectOptions(
      await canvas.findByLabelText(
        'Select a stored procedure',
        {},
        { timeout: 4000 }
      ),
      'stored_procedure_1'
    );

    fireEvent.click(await canvas.findByText('Add new argument'));
    await userEvent.type(canvas.getByTestId('arguments[0].name'), 'id');
    await userEvent.selectOptions(
      canvas.getByTestId('arguments[0].type'),
      'int'
    );

    await userEvent.selectOptions(
      await canvas.findByLabelText('Return Type', {}, { timeout: 4000 }),
      'logical_model_1'
    );

    fireEvent.click(canvas.getByText('Advanced'));
    await userEvent.type(
      await canvas.findByLabelText('Custom Name'),
      'my_custom_name'
    );

    fireEvent.click(canvas.getByText('Track Stored Procedure'));

    await expect(
      await canvas.findByText(STORED_PROCEDURE_TRACK_SUCCESS)
    ).toBeInTheDocument();
  },
};

export const ErrorWhileSaving: StoryObj<typeof StoredProcedureWidget> = {
  render: args => {
    return <StoredProcedureWidget />;
  },

  name: '🧪 Error while saving',

  parameters: {
    msw: handlers['400'],
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.selectOptions(
      await canvas.findByLabelText('Select a source', {}, { timeout: 4000 }),
      'bikes'
    );

    await userEvent.selectOptions(
      await canvas.findByLabelText(
        'Select a stored procedure',
        {},
        { timeout: 4000 }
      ),
      'stored_procedure_1'
    );

    fireEvent.click(await canvas.findByText('Add new argument'));
    await userEvent.type(canvas.getByTestId('arguments[0].name'), 'id');
    await userEvent.selectOptions(
      canvas.getByTestId('arguments[0].type'),
      'int'
    );

    await userEvent.selectOptions(
      await canvas.findByLabelText('Return Type', {}, { timeout: 4000 }),
      'logical_model_1'
    );

    fireEvent.click(canvas.getByText('Track Stored Procedure'));

    await expect(
      await canvas.findByText(STORED_PROCEDURE_TRACK_ERROR)
    ).toBeInTheDocument();
  },
};

export const InternalErrorIntrospection: StoryObj<
  typeof StoredProcedureWidget
> = {
  render: args => {
    return <StoredProcedureWidget />;
  },

  name: '🧪 Error while introspection',

  parameters: {
    msw: handlers['500'],
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.selectOptions(
      await canvas.findByLabelText('Select a source', {}, { timeout: 4000 }),
      'bikes'
    );

    await waitFor(
      async () => {
        await expect(await canvas.findByTestId('Error')).toBeInTheDocument();
      },
      { timeout: 10000 }
    );
  },
};
