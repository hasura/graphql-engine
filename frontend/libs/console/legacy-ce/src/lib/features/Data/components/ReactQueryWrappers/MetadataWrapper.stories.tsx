import { expect } from '@storybook/jest';
import { Meta, StoryObj } from '@storybook/react';
import { waitFor, within } from '@storybook/testing-library';
import { rest } from 'msw';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { metadataWithSourcesAndTables } from '../../../hasura-metadata-api/mocks/metadata.mock';
import { MetadataWrapper } from './MetadataWrapper';
import { TestIds as StatusTestIds } from './ReactQueryStatusUI';
import { TestIds } from './ReactQueryUIWrapper';
import { checkForStatusElements } from './story-utils';
const LOADING_TIME = 2000;

const waitForLoading = async () =>
  await waitFor(() => new Promise(res => setTimeout(res, LOADING_TIME)), {
    timeout: LOADING_TIME + 1000,
  });

export default {
  component: MetadataWrapper,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: [
      rest.post(`http://localhost:8080/v1/metadata`, async (req, res, ctx) => {
        return res(
          ctx.status(200),
          ctx.delay(LOADING_TIME),
          ctx.json(metadataWithSourcesAndTables)
        );
      }),
    ],
  },
} satisfies Meta<typeof MetadataWrapper>;

export const Basic: StoryObj<typeof MetadataWrapper> = {
  render: () => {
    return (
      <div>
        <MetadataWrapper
          render={({ data }) => <pre>{JSON.stringify(data, null, 2)}</pre>}
        />
      </div>
    );
  },
  play: async ({ canvasElement }) => {
    await checkForStatusElements({
      displayed: [StatusTestIds.spinner],
      childContentId: TestIds.renderContent,
      canvasElement,
    });

    await waitForLoading();

    await checkForStatusElements({
      displayed: [TestIds.renderContent],
      childContentId: TestIds.renderContent,
      canvasElement,
    });
  },
};

export const WithSelector: StoryObj<typeof MetadataWrapper> = {
  render: () => {
    return (
      <div>
        <MetadataWrapper
          selector={m => m.metadata.sources}
          render={({ data }) => (
            <div>
              Metadata Loaded! Source Name: <pre>{data[0].name}</pre>
            </div>
          )}
        />
      </div>
    );
  },
  play: async ({ canvasElement }) => {
    const c = within(canvasElement);

    await checkForStatusElements({
      displayed: [StatusTestIds.spinner],
      childContentId: TestIds.renderContent,
      canvasElement,
    });

    await waitForLoading();

    await checkForStatusElements({
      displayed: [TestIds.renderContent],
      childContentId: TestIds.renderContent,
      canvasElement,
    });

    await expect(await c.findByText('sqlite_test')).toBeInTheDocument();
  },
};
