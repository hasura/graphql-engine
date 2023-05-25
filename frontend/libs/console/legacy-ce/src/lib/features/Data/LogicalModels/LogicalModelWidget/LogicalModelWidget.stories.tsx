import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { LogicalModelWidget } from './LogicalModelWidget';
import { ReduxDecorator } from '../../../../storybook/decorators/redux-decorator';
import { fireEvent, userEvent, within } from '@storybook/testing-library';
import { handlers } from './mocks/handlers';
import { expect } from '@storybook/jest';
import { defaultEmptyValues } from './validationSchema';

export default {
  component: LogicalModelWidget,
  decorators: [
    ReactQueryDecorator(),
    ReduxDecorator({
      tables: {
        dataHeaders: {
          'x-hasura-admin-secret': 'myadminsecretkey',
        } as any,
      },
    }),
  ],
} as Meta<typeof LogicalModelWidget>;

export const DefaultView: StoryObj<typeof LogicalModelWidget> = {
  render: () => <LogicalModelWidget />,

  parameters: {
    msw: handlers['200'],
  },
};

export const DialogVariant: StoryObj<typeof LogicalModelWidget> = {
  render: () => <LogicalModelWidget asDialog />,

  parameters: {
    msw: handlers['200'],
  },
};

export const PreselectedAndDisabledInputs: StoryObj<typeof LogicalModelWidget> =
  {
    render: () => (
      <LogicalModelWidget
        defaultValues={{
          ...defaultEmptyValues,
          dataSourceName: 'chinook',
        }}
        disabled={{
          dataSourceName: true,
        }}
      />
    ),

    parameters: {
      msw: handlers['200'],
    },
  };

export const BasicUserFlow: StoryObj<typeof LogicalModelWidget> = {
  render: () => <LogicalModelWidget />,

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
      canvas.getByTestId('fields[0].type'),
      'integer'
    );

    fireEvent.click(canvas.getByText('Add Field'));

    await userEvent.type(canvas.getByTestId('fields[1].name'), 'name');
    await userEvent.selectOptions(canvas.getByTestId('fields[1].type'), 'text');

    fireEvent.click(canvas.getByText('Create Logical Model'));

    await expect(
      await canvas.findByText('Successfully tracked Logical Model')
    ).toBeInTheDocument();
  },
};

export const NetworkErrorOnSubmit: StoryObj<typeof LogicalModelWidget> = {
  render: () => <LogicalModelWidget />,
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
      canvas.getByTestId('fields[0].type'),
      'integer'
    );

    fireEvent.click(canvas.getByText('Add Field'));

    await userEvent.type(canvas.getByTestId('fields[1].name'), 'name');
    await userEvent.selectOptions(canvas.getByTestId('fields[1].type'), 'text');

    fireEvent.click(canvas.getByText('Create Logical Model'));

    await expect(
      await canvas.findByText(`Logical model 'foobar' is already tracked.`)
    ).toBeInTheDocument();
  },
};
