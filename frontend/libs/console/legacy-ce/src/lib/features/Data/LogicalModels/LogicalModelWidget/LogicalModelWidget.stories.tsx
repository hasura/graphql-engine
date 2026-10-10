import { expect, userEvent, waitFor, within, fireEvent } from 'storybook/test';
import { Meta, StoryObj } from '@storybook/react-webpack5';
import {
  ConsoleTypeDecorator,
  ReactQueryDecorator,
} from '@hasura/shared/testing';
import { waitForSpinnerOverlay } from '../../components/ReactQueryWrappers/story-utils';
import {
  LOGICAL_MODEL_CREATE_ERROR,
  LOGICAL_MODEL_CREATE_SUCCESS,
} from '../constants';
import { LogicalModelWidget } from './LogicalModelWidget';
import { handlers } from './mocks/handlers';
import { defaultEmptyValues } from './validationSchema';

export default {
  component: LogicalModelWidget,
  decorators: [
    ConsoleTypeDecorator({ consoleType: 'pro' }),
    ReactQueryDecorator(),
  ],
} as Meta<typeof LogicalModelWidget>;

export const DefaultView: StoryObj<typeof LogicalModelWidget> = {
  parameters: {
    msw: handlers['200'],
  },
};

export const DialogVariant: StoryObj<typeof LogicalModelWidget> = {
  args: {
    asDialog: true,
  },

  parameters: {
    msw: handlers['200'],
  },
};

export const PreselectedAndDisabledInputs: StoryObj<typeof LogicalModelWidget> =
  {
    args: {
      defaultValues: {
        ...defaultEmptyValues,
        dataSourceName: 'chinook',
      },
      disabled: {
        dataSourceName: true,
      },
    },

    parameters: {
      msw: handlers['200'],
    },
  };

export const BasicUserFlow: StoryObj<typeof LogicalModelWidget> = {
  name: '🧪 Basic user flow',

  parameters: {
    msw: handlers['200'],
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await canvas.findByTestId('dataSourceName-chinook');

    await userEvent.selectOptions(
      await canvas.findByLabelText('Select a source', {}, { timeout: 4000 }),
      'chinook',
    );

    // wait for spinner overlay from UI provider to be out of document
    await waitForSpinnerOverlay(canvasElement);

    await userEvent.type(
      canvas.getByPlaceholderText('Enter a name for your Logical Model'),
      'foobar',
    );

    await userEvent.click(canvas.getByText('Add Field'), {});

    await userEvent.type(canvas.getByTestId('fields[0].name'), 'id');
    await userEvent.selectOptions(
      canvas.getByTestId('fields-input-type-0'),
      'scalar:integer',
    );

    await userEvent.click(canvas.getByText('Add Field'));

    await userEvent.type(canvas.getByTestId('fields[1].name'), 'name');
    await userEvent.selectOptions(
      canvas.getByTestId('fields-input-type-1'),
      'scalar:text',
    );

    await userEvent.click(canvas.getByText('Create'));

    await expect(
      await canvas.findByText(LOGICAL_MODEL_CREATE_SUCCESS),
    ).toBeInTheDocument();
  },
};

export const NetworkErrorOnSubmit: StoryObj<typeof LogicalModelWidget> = {
  name: '🧪 Network Error On Submit',

  parameters: {
    msw: handlers['400'],
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await canvas.findByTestId('dataSourceName-chinook');
    await userEvent.selectOptions(
      await canvas.findByLabelText('Select a source', {}, { timeout: 4000 }),
      'chinook',
    );

    // wait for spinner overlay from UI provider to be out of document
    await waitForSpinnerOverlay(canvasElement);

    await userEvent.type(
      await canvas.findByLabelText('Logical Model Name', {}, { timeout: 4000 }),
      'foobar',
    );

    await waitFor(async () =>
      // button is disabled if there's no source selected and will be enabled once sources load:
      expect(canvas.getByRole('button', { name: /Add Field/i })).toBeEnabled(),
    );

    await fireEvent.click(canvas.getByText('Add Field'));

    await userEvent.type(canvas.getByTestId('fields[0].name'), 'id');
    await userEvent.selectOptions(
      canvas.getByTestId('fields-input-type-0'),
      'scalar:integer',
    );

    await fireEvent.click(canvas.getByText('Add Field'));

    await userEvent.type(canvas.getByTestId('fields[1].name'), 'name');
    await userEvent.selectOptions(
      canvas.getByTestId('fields-input-type-1'),
      'scalar:text',
    );

    await fireEvent.click(canvas.getByText('Create'));

    await expect(
      await canvas.findByText(LOGICAL_MODEL_CREATE_ERROR, { exact: false }),
    ).toBeInTheDocument();
  },
};
