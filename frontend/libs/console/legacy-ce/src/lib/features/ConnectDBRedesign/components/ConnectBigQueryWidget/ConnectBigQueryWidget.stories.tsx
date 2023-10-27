import { StoryObj, StoryFn, Meta } from '@storybook/react';
import { ConnectBigQueryWidget } from './ConnectBigQueryWidget';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';

export default {
  component: ConnectBigQueryWidget,
  decorators: [ReactQueryDecorator()],
  parameters: {
    // msw: handlers(),
  },
} as Meta<typeof ConnectBigQueryWidget>;

export const CreateConnection: StoryFn<typeof ConnectBigQueryWidget> = () => {
  return (
    <div className="flex justify-center">
      <div className="w-1/2">
        <ConnectBigQueryWidget />
      </div>
    </div>
  );
};

export const Test: StoryObj<typeof ConnectBigQueryWidget> = {
  render: () => {
    return (
      <div className="flex justify-center">
        <div className="w-1/2">
          <ConnectBigQueryWidget />
        </div>
      </div>
    );
  },

  name: '🧪 BigQuery Interaction test (add database)',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // verify if the right title is displayed. It should contain the word `postgres`.
    await expect(
      await canvas.findByText('Connect BigQuery Database')
    ).toBeInTheDocument();

    // verify if all the fields are present (in oss mode)

    await expect(
      await canvas.findByLabelText('Database name')
    ).toBeInTheDocument();

    // There should be exactly 2 supported database connection options
    const radioOptions = await canvas.findAllByLabelText(
      'Connect Database via'
    );
    await expect(radioOptions.length).toBe(2);

    const serviceAccountKeyOption = await canvas.findByTestId(
      'configuration.serviceAccount.type-serviceAccountKey'
    );
    await expect(serviceAccountKeyOption).toBeInTheDocument();

    const placeholders = await canvas.findAllByPlaceholderText(
      'HASURA_GRAPHQL_DB_URL_FROM_ENV'
    );
    await expect(placeholders.length).toBe(3);

    await expect(
      await canvas.findByTestId('configuration.projectId.type-value')
    ).toBeInTheDocument();
    await expect(
      await canvas.findByTestId('configuration.projectId.type-envVar')
    ).toBeInTheDocument();

    await expect(
      await canvas.findByTestId('configuration.datasets.type-value')
    ).toBeInTheDocument();
    await expect(
      await canvas.findByTestId('configuration.datasets.type-envVar')
    ).toBeInTheDocument();
  },
};
