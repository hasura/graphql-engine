import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ConnectBigQueryWidget } from './ConnectBigQueryWidget';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { userEvent, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';

export default {
  component: ConnectBigQueryWidget,
  decorators: [ReactQueryDecorator()],
  parameters: {
    // msw: handlers(),
  },
} as ComponentMeta<typeof ConnectBigQueryWidget>;

export const CreateConnection: ComponentStory<
  typeof ConnectBigQueryWidget
> = () => {
  return (
    <div className="flex justify-center">
      <div className="w-1/2">
        <ConnectBigQueryWidget />
      </div>
    </div>
  );
};

export const Test: ComponentStory<typeof ConnectBigQueryWidget> = () => {
  return (
    <div className="flex justify-center">
      <div className="w-1/2">
        <ConnectBigQueryWidget />
      </div>
    </div>
  );
};

Test.storyName = '🧪 BigQuery Interaction test (add database)';
Test.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  // verify if the right title is displayed. It should contain the word `postgres`.
  expect(
    await canvas.findByText('Connect BigQuery Database')
  ).toBeInTheDocument();

  // verify if all the fields are present (in oss mode)

  expect(await canvas.findByLabelText('Database name')).toBeInTheDocument();

  // There should be exactly 2 supported database connection options
  const radioOptions = await canvas.findAllByLabelText('Connect Database via');
  expect(radioOptions.length).toBe(2);

  const serviceAccountKeyOption = await canvas.findByTestId(
    'configuration.serviceAccount.type-serviceAccountKey'
  );
  expect(serviceAccountKeyOption).toBeInTheDocument();

  // click on the environment variable option and verify if the correct fields are shown
  const environmentVariableOption = await canvas.findByTestId(
    'configuration.serviceAccount.type-envVar'
  );
  expect(environmentVariableOption).toBeChecked();
  userEvent.click(environmentVariableOption);
  expect(
    await canvas.findByPlaceholderText('HASURA_GRAPHQL_DB_URL_FROM_ENV')
  ).toBeInTheDocument();

  expect(
    await canvas.findByTestId('configuration.projectId.type-value')
  ).toBeInTheDocument();
  expect(
    await canvas.findByTestId('configuration.projectId.type-envVar')
  ).toBeInTheDocument();

  expect(
    await canvas.findByTestId('configuration.datasets.type-value')
  ).toBeInTheDocument();
  expect(
    await canvas.findByTestId('configuration.datasets.type-envVar')
  ).toBeInTheDocument();
};
