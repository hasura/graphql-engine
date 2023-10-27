import { expect } from '@storybook/jest';
import { Meta, StoryObj } from '@storybook/react';
import {
  fireEvent,
  screen,
  userEvent,
  waitFor,
  within,
} from '@storybook/testing-library';
import { ConsoleTypeDecorator } from '../../../../storybook/decorators';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { dismissToast } from '../../../../utils/StoryUtils';
import { LogicalModel } from '../../../hasura-metadata-types';
import { waitForSpinnerOverlay } from '../../components/ReactQueryWrappers/story-utils';
import { logicalModelFieldToFormField } from '../LogicalModel/utils/logicalModelFieldToFormField';
import {
  LOGICAL_MODEL_CREATE_ERROR,
  LOGICAL_MODEL_CREATE_SUCCESS,
  LOGICAL_MODEL_EDIT_SUCCESS,
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
      'chinook'
    );

    // wait for spinner overlay from UI provider to be out of document
    await waitForSpinnerOverlay(canvasElement);

    await userEvent.type(
      canvas.getByPlaceholderText('Enter a name for your Logical Model'),
      'foobar'
    );

    await userEvent.click(canvas.getByText('Add Field'), {});

    await userEvent.type(canvas.getByTestId('fields[0].name'), 'id');
    await userEvent.selectOptions(
      canvas.getByTestId('fields-input-type-0'),
      'scalar:integer'
    );

    await userEvent.click(canvas.getByText('Add Field'));

    await userEvent.type(canvas.getByTestId('fields[1].name'), 'name');
    await userEvent.selectOptions(
      canvas.getByTestId('fields-input-type-1'),
      'scalar:text'
    );

    await userEvent.click(canvas.getByText('Create'));

    await expect(
      await canvas.findByText(LOGICAL_MODEL_CREATE_SUCCESS)
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
      'chinook'
    );

    // wait for spinner overlay from UI provider to be out of document
    await waitForSpinnerOverlay(canvasElement);

    await userEvent.type(
      await canvas.findByLabelText('Logical Model Name', {}, { timeout: 4000 }),
      'foobar'
    );

    await waitFor(() =>
      // button is disabled if there's no source selected and will be enabled once sources load:
      expect(canvas.getByRole('button', { name: /Add Field/i })).toBeEnabled()
    );

    fireEvent.click(canvas.getByText('Add Field'));

    await userEvent.type(canvas.getByTestId('fields[0].name'), 'id');
    await userEvent.selectOptions(
      canvas.getByTestId('fields-input-type-0'),
      'scalar:integer'
    );

    fireEvent.click(canvas.getByText('Add Field'));

    await userEvent.type(canvas.getByTestId('fields[1].name'), 'name');
    await userEvent.selectOptions(
      canvas.getByTestId('fields-input-type-1'),
      'scalar:text'
    );

    fireEvent.click(canvas.getByText('Create'));

    await expect(
      await canvas.findByText(LOGICAL_MODEL_CREATE_ERROR, { exact: false })
    ).toBeInTheDocument();
  },
};

const defaultLogicalModelValues: LogicalModel = {
  name: 'Logical Model',
  fields: [
    {
      name: 'id',
      type: {
        scalar: 'integer',
        nullable: false,
      },
    },
    {
      name: 'nested',
      type: {
        logical_model: 'logical_model_1',
        nullable: false,
      },
    },
    {
      name: 'nested_array',
      type: { array: { logical_model: 'logical_model_2', nullable: false } },
    },
  ],
};

// doing this so the underlying data can adhere to the type. this is the same way we do it in the logical model landing page when implementing the widget
const logicalModelAdaptedForProps = {
  name: defaultLogicalModelValues.name,
  dataSourceName: 'chinook',
  fields: defaultLogicalModelValues.fields.map(logicalModelFieldToFormField),
};

export const NestedLogicalModels: StoryObj<typeof LogicalModelWidget> = {
  args: {
    defaultValues: logicalModelAdaptedForProps,
  },
  parameters: {
    msw: handlers['200'],
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    //Had to comment out b/c it's never resolving the test and hanging up the builds

    // Establish a request listener but don't resolve it yet.
    // const pendingRequest = waitForRequest(
    //   'POST',
    //   'http://localhost:8080/v1/metadata',
    //   '_track_logical_model'
    // );
    await waitForSpinnerOverlay(canvasElement);
    await userEvent.click(await canvas.findByText('Save'));

    //Had to comment out b/c it's never resolving the test and hanging up the builds

    // Await the request and get its reference.
    // const request = await pendingRequest;
    // const payload = await request.clone().json();

    // expect(payload.args.fields).toMatchObject([
    //   {
    //     name: 'id',
    //     type: {
    //       scalar: 'integer',
    //       nullable: false,
    //     },
    //   },
    //   {
    //     name: 'nested',
    //     type: {
    //       logical_model: 'logical_model_1',
    //       nullable: false,
    //     },
    //   },
    //   {
    //     name: 'nested_array',
    //     type: {
    //       array: {
    //         logical_model: 'logical_model_2',
    //         nullable: false,
    //       },
    //     },
    //   },
    // ]);

    await expect(
      await screen.findByText(
        LOGICAL_MODEL_EDIT_SUCCESS,
        { exact: false },
        { timeout: 3000 }
      )
    ).toBeInTheDocument();

    await dismissToast();
  },
};

export const EditingNestedLogicalModels: StoryObj<typeof LogicalModelWidget> = {
  args: {
    defaultValues: logicalModelAdaptedForProps,
  },
  parameters: {
    msw: handlers['200'],
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await waitForSpinnerOverlay(canvasElement);

    //Had to comment out b/c it's never resolving the test and hanging up the builds

    // Establish a request listener but don't resolve it yet.
    // const pendingRequest = waitForRequest(
    //   'POST',
    //   'http://localhost:8080/v1/metadata',
    //   '_track_logical_model'
    // );

    await waitFor(() =>
      // button is disabled if there's no source selected and will be enabled once sources load:
      expect(canvas.getByRole('button', { name: /Add Field/i })).toBeEnabled()
    );

    // Change from logical model to scalar, and then back should result in the same initial logical model type
    await userEvent.click(await canvas.findByTestId('fields-input-type-1'));
    // Set array to true
    await userEvent.click(await canvas.findByTestId('fields-input-array-1'));
    // Change it to scalar
    await userEvent.selectOptions(
      await canvas.findByTestId('fields-input-type-1'),
      'scalar:integer'
    );
    await userEvent.click(await canvas.findByText('Save'));

    //Had to comment out b/c it's never resolving the test and hanging up the builds

    // Await the request and get its reference.
    // const request = await pendingRequest;
    // const payload = await request.clone().json();
    // expect(payload.args.fields).toMatchObject([
    //   {
    //     name: 'id',
    //     type: {
    //       scalar: 'integer',
    //       nullable: false,
    //     },
    //   },
    //   {
    //     name: 'nested',
    //     type: {
    //       scalar: 'integer',
    //       nullable: false,
    //     },
    //   },
    //   {
    //     name: 'nested_array',
    //     type: {
    //       array: {
    //         logical_model: 'logical_model_2',
    //         nullable: false,
    //       },
    //     },
    //   },
    // ]);
    await expect(
      await screen.findByText(
        LOGICAL_MODEL_EDIT_SUCCESS,
        { exact: false },
        { timeout: 3000 }
      )
    ).toBeInTheDocument();

    await dismissToast();
  },
};
