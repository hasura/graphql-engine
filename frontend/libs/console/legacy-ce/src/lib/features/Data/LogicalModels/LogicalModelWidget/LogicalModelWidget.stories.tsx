import { ComponentMeta, ComponentStory } from '@storybook/react';
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
} as ComponentMeta<typeof LogicalModelWidget>;

export const DefaultView: ComponentStory<typeof LogicalModelWidget> = () => (
  <LogicalModelWidget />
);

DefaultView.parameters = {
  msw: handlers['200'],
};

export const DialogVariant: ComponentStory<typeof LogicalModelWidget> = () => (
  <LogicalModelWidget asDialog />
);
DialogVariant.parameters = {
  msw: handlers['200'],
};

export const PreselectedAndDisabledInputs: ComponentStory<
  typeof LogicalModelWidget
> = () => (
  <LogicalModelWidget
    defaultValues={{
      ...defaultEmptyValues,
      dataSourceName: 'chinook',
    }}
    disabled={{
      dataSourceName: true,
    }}
  />
);
PreselectedAndDisabledInputs.parameters = {
  msw: handlers['200'],
};

export const BasicUserFlow: ComponentStory<typeof LogicalModelWidget> = () => (
  <LogicalModelWidget />
);

BasicUserFlow.storyName = '🧪 Basic user flow';
BasicUserFlow.parameters = {
  msw: handlers['200'],
};
BasicUserFlow.play = async ({ canvasElement }) => {
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
};

export const NetworkErrorOnSubmit: ComponentStory<
  typeof LogicalModelWidget
> = () => <LogicalModelWidget />;

NetworkErrorOnSubmit.storyName = '🧪 Network Error On Submit';
NetworkErrorOnSubmit.parameters = {
  msw: handlers['400'],
};
NetworkErrorOnSubmit.play = async ({ canvasElement }) => {
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
};
