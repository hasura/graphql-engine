import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { LogicalModelWidget } from './LogicalModelWidget';
import { fireEvent, userEvent, within } from '@storybook/testing-library';
import { handlers } from './mocks/handlers';
import { expect } from '@storybook/jest';
import { defaultEmptyValues } from './validationSchema';
import { waitForRequest } from '../../../../storybook/utils/waitForRequest';

export default {
  component: LogicalModelWidget,
  decorators: [ReactQueryDecorator()],
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
    await userEvent.selectOptions(
      await canvas.findByLabelText('Select a source', {}, { timeout: 4000 }),
      'chinook'
    );
    await userEvent.type(
      await canvas.findByLabelText('Logical Model Name', {}, { timeout: 4000 }),
      'foobar'
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

    fireEvent.click(canvas.getByText('Create Logical Model'));

    await expect(
      await canvas.findByText('Successfully tracked Logical Model')
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
    await userEvent.selectOptions(
      await canvas.findByLabelText('Select a source', {}, { timeout: 4000 }),
      'chinook'
    );
    await userEvent.type(
      await canvas.findByLabelText('Logical Model Name', {}, { timeout: 4000 }),
      'foobar'
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

    fireEvent.click(canvas.getByText('Create Logical Model'));

    await expect(
      await canvas.findByText(`Logical model 'foobar' is already tracked.`)
    ).toBeInTheDocument();
  },
};

const defaultLogicalModelValues = {
  name: 'Logical Model',
  dataSourceName: 'chinook',
  fields: [
    {
      name: 'id',
      type: 'integer',
      array: false,
      nullable: false,
      typeClass: 'scalar' as const,
    },
    {
      name: 'nested',
      type: 'logical_model_1',
      array: false,
      nullable: false,
      typeClass: 'logical_model' as const,
    },
    {
      name: 'nested_array',
      type: 'logical_model_2',
      array: true,
      nullable: false,
      typeClass: 'logical_model' as const,
    },
  ],
};

export const NestedLogicalModels: StoryObj<typeof LogicalModelWidget> = {
  args: {
    defaultValues: defaultLogicalModelValues,
  },
  parameters: {
    msw: handlers['200'],
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    // Establish a request listener but don't resolve it yet.
    const pendingRequest = waitForRequest(
      'POST',
      'http://localhost:8080/v1/metadata',
      '_track_logical_model'
    );
    fireEvent.click(await canvas.findByText('Edit Logical Model'));
    // Await the request and get its reference.
    const request = await pendingRequest;
    const payload = await request.clone().json();
    expect(payload.args.fields).toMatchObject([
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
        type: {
          array: {
            logical_model: 'logical_model_2',
            nullable: false,
          },
        },
      },
    ]);
  },
};

export const EditingNestedLogicalModels: StoryObj<typeof LogicalModelWidget> = {
  args: {
    defaultValues: defaultLogicalModelValues,
  },
  parameters: {
    msw: handlers['200'],
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    // Establish a request listener but don't resolve it yet.
    const pendingRequest = waitForRequest(
      'POST',
      'http://localhost:8080/v1/metadata',
      '_track_logical_model'
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
    fireEvent.click(await canvas.findByText('Edit Logical Model'));
    // Await the request and get its reference.
    const request = await pendingRequest;
    const payload = await request.clone().json();
    expect(payload.args.fields).toMatchObject([
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
          scalar: 'integer',
          nullable: false,
        },
      },
      {
        name: 'nested_array',
        type: {
          array: {
            logical_model: 'logical_model_2',
            nullable: false,
          },
        },
      },
    ]);
  },
};
