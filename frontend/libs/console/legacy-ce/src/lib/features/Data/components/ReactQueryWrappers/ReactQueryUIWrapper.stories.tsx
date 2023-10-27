import { expect } from '@storybook/jest';
import { Meta, StoryObj } from '@storybook/react';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { useState } from 'react';
import { useQuery } from 'react-query';
import { APIError } from '../../../../hooks/error';
import { Button } from '../../../../new-components/Button';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { TestIds as StatusTestIds } from './ReactQueryStatusUI';
import {
  IdleQueryErrorMessage,
  ReactQueryUIWrapper,
  TestIds,
} from './ReactQueryUIWrapper';
import { checkForStatusElements, shorterTextContent } from './story-utils';

export default {
  component: ReactQueryUIWrapper,
  decorators: [ReactQueryDecorator()],
} satisfies Meta<typeof ReactQueryUIWrapper>;

const demoData = {
  foo: 'bar',
  arrayProperty: ['hello', 'goodbye'],
};

const errorMessage = 'Error message';

const successFn = async () => {
  await pauseForLoading();
  return demoData;
};
const errorFn = async () => {
  await pauseForLoading();
  throw new Error(errorMessage);
};

const LOADING_TIME = 2000;
const pauseForLoading = () =>
  new Promise(resolve => setTimeout(resolve, LOADING_TIME));

const waitForLoading = async () =>
  await waitFor(async () => await pauseForLoading(), {
    timeout: LOADING_TIME + 1000,
  });

export const Basic: StoryObj<typeof ReactQueryUIWrapper> = {
  render: () => {
    const queryReturn = useQuery<typeof demoData, APIError>({
      queryKey: ['foo'],
      queryFn: successFn,
      //enabled: false,
      //retry: false,
    });

    return (
      <div>
        <div className="p-4 mb-4 border-b font-semibold">
          Status: {queryReturn.status}
        </div>
        <ReactQueryUIWrapper
          useQueryResult={queryReturn}
          render={({ data }) => (
            <div>
              <div>
                <div>{shorterTextContent}</div>
              </div>
              <div>Data:</div>
              <div>{JSON.stringify(data)}</div>
            </div>
          )}
        />
      </div>
    );
  },
  play: async ({ canvasElement }) => {
    await checkForStatusElements({
      displayed: [StatusTestIds.spinner],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    await waitForLoading();

    await checkForStatusElements({
      displayed: [TestIds.renderContent],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    // make sure that the data is rendered as expected
    await expect(
      within(canvasElement).getByText(JSON.stringify(demoData))
    ).toBeInTheDocument();
  },
};
export const SkeletonInsteadOfContent: typeof Basic = {
  render: () => {
    const queryReturn = useQuery<typeof demoData, APIError>({
      queryKey: ['foo'],
      queryFn: successFn,
    });

    return (
      <div>
        <div className="p-4 mb-4 border-b font-semibold">
          Status: {queryReturn.status}
        </div>
        <ReactQueryUIWrapper
          loader="skeleton"
          skeletonProps={{ count: 3, height: 30 }}
          useQueryResult={queryReturn}
          render={({ data }) => (
            <div>
              <div>
                <div>{shorterTextContent}</div>
              </div>
              <div>Data:</div>
              <div>{JSON.stringify(data)}</div>
            </div>
          )}
        />
      </div>
    );
  },
  play: async ({ canvasElement }) => {
    await checkForStatusElements({
      displayed: [StatusTestIds.skeleton],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    await waitForLoading();

    await checkForStatusElements({
      displayed: [TestIds.renderContent],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    // make sure that the data is rendered as expected
    await expect(
      within(canvasElement).getByText(JSON.stringify(demoData))
    ).toBeInTheDocument();
  },
};
export const SkeletonOverlay: typeof Basic = {
  render: () => {
    const queryReturn = useQuery<typeof demoData, APIError>({
      queryKey: ['foo'],
      queryFn: successFn,
    });

    return (
      <div>
        <div className="p-4 mb-4 border-b font-semibold">
          Status: {queryReturn.status}
        </div>
        <ReactQueryUIWrapper
          loadingStyle="overlay"
          loader="skeleton"
          skeletonCount={3}
          fallbackData={demoData}
          useQueryResult={queryReturn}
          render={({ data }) => (
            <div>
              <div>
                <div>{shorterTextContent}</div>
              </div>
              <div>Data:</div>
              <div>{JSON.stringify(data)}</div>
            </div>
          )}
        />
      </div>
    );
  },
  play: async ({ canvasElement }) => {
    await checkForStatusElements({
      displayed: [
        StatusTestIds.skeleton,
        StatusTestIds.loadingOverlay,
        TestIds.renderContent,
      ],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    await waitForLoading();

    await checkForStatusElements({
      displayed: [TestIds.renderContent],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    // make sure that the data is rendered as expected
    await expect(
      within(canvasElement).getByText(JSON.stringify(demoData))
    ).toBeInTheDocument();
  },
};
export const SpinnerOverlay: typeof Basic = {
  render: () => {
    const queryReturn = useQuery<typeof demoData, APIError>({
      queryKey: ['foo'],
      queryFn: successFn,
    });

    return (
      <div>
        <div className="p-4 mb-4 border-b font-semibold">
          Status: {queryReturn.status}
        </div>
        <ReactQueryUIWrapper
          loadingStyle="overlay"
          loader="spinner"
          miniSpinnerBackdrop
          fallbackData={demoData}
          useQueryResult={queryReturn}
          render={({ data }) => (
            <div>
              <div>
                <div>{shorterTextContent}</div>
              </div>
              <div>Data:</div>
              <div>{JSON.stringify(data)}</div>
            </div>
          )}
        />
      </div>
    );
  },
  play: async ({ canvasElement }) => {
    await checkForStatusElements({
      displayed: [
        StatusTestIds.spinner,
        StatusTestIds.loadingOverlay,
        TestIds.renderContent,
      ],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    await waitForLoading();

    await checkForStatusElements({
      displayed: [TestIds.renderContent],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    // make sure that the data is rendered as expected
    await expect(
      within(canvasElement).getByText(JSON.stringify(demoData))
    ).toBeInTheDocument();
  },
};

export const ErrorDisplay: typeof Basic = {
  render: () => {
    const queryReturn = useQuery<typeof demoData, APIError>({
      queryKey: ['foo'],
      queryFn: errorFn,
      retry: false,
    });

    return (
      <div>
        <div className="p-4 mb-4 border-b font-semibold">
          Status: {queryReturn.status}
        </div>
        <ReactQueryUIWrapper
          useQueryResult={queryReturn}
          render={({ data }) => (
            <div>
              <div>
                <div>{shorterTextContent}</div>
              </div>
              <div>Data:</div>
              <div>{JSON.stringify(data)}</div>
            </div>
          )}
        />
      </div>
    );
  },
  play: async ({ canvasElement }) => {
    await checkForStatusElements({
      displayed: [StatusTestIds.spinner],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    await waitForLoading();

    await checkForStatusElements({
      displayed: [StatusTestIds.error],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    // make sure that the data is rendered as expected
    await expect(
      within(canvasElement).queryByText(JSON.stringify(demoData))
    ).not.toBeInTheDocument();
  },
};

export const ErrorBelowContent: typeof Basic = {
  render: () => {
    const queryReturn = useQuery<typeof demoData, APIError>({
      queryKey: ['foo'],
      queryFn: errorFn,
      retry: false,
    });

    return (
      <div>
        <div className="p-4 mb-4 border-b font-semibold">
          Status: {queryReturn.status}
        </div>
        <ReactQueryUIWrapper
          useQueryResult={queryReturn}
          fallbackData={demoData}
          errorStyle="below-content"
          render={({ data }) => (
            <div>
              <div>
                <div>{shorterTextContent}</div>
              </div>
              <div>Data:</div>
              <div>{JSON.stringify(data)}</div>
            </div>
          )}
        />
      </div>
    );
  },
  play: async ({ canvasElement }) => {
    await checkForStatusElements({
      displayed: [StatusTestIds.spinner],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    await waitForLoading();

    await checkForStatusElements({
      displayed: [
        StatusTestIds.error,
        StatusTestIds.errorBelow,
        TestIds.renderContent,
      ],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    // make sure that the data is rendered as expected
    await expect(
      within(canvasElement).queryByText(JSON.stringify(demoData))
    ).toBeInTheDocument();
  },
};

export const CustomErrorBelowContent: typeof Basic = {
  render: () => {
    const queryReturn = useQuery<typeof demoData, APIError>({
      queryKey: ['foo'],
      queryFn: errorFn,
      retry: false,
    });

    return (
      <div>
        <div className="p-4 mb-4 border-b font-semibold">
          Status: {queryReturn.status}
        </div>
        <ReactQueryUIWrapper
          useQueryResult={queryReturn}
          fallbackData={demoData}
          errorStyle="below-content"
          renderError={({ error }) => (
            <div className="text-red-600">Message: {error?.message}</div>
          )}
          render={({ data }) => (
            <div>
              <div>
                <div>{shorterTextContent}</div>
              </div>
              <div>Data:</div>
              <div>{JSON.stringify(data)}</div>
            </div>
          )}
        />
      </div>
    );
  },
  play: async ({ canvasElement }) => {
    await checkForStatusElements({
      displayed: [StatusTestIds.spinner],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    await waitForLoading();

    await checkForStatusElements({
      displayed: [
        StatusTestIds.errorBelow,
        StatusTestIds.customError,
        TestIds.renderContent,
      ],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    // make sure that the data is rendered as expected
    await expect(
      within(canvasElement).queryByText(JSON.stringify(demoData))
    ).toBeInTheDocument();
  },
};

export const IdleQueryWithFallbackData: typeof Basic = {
  name: 'Idle Query - Show Content with Fallback Data',
  render: () => {
    const [enabled, setEnabled] = useState(false);
    const queryReturn = useQuery<typeof demoData, APIError>({
      queryKey: ['foo'],
      queryFn: successFn,
      enabled,
    });

    return (
      <div>
        <div className="p-4 mb-4 border-b font-semibold">
          Status: {queryReturn.status}
          <br />
          <Button
            data-testid="enable-query"
            onClick={() => setEnabled(e => !e)}
          >
            Toggle Enabled
          </Button>
        </div>
        <ReactQueryUIWrapper
          fallbackData={demoData}
          useQueryResult={queryReturn}
          render={({ data: result }) => (
            <div>
              <div>
                <div>{shorterTextContent}</div>
              </div>
              <div>Data:</div>
              <div>{JSON.stringify(result)}</div>
            </div>
          )}
        />
      </div>
    );
  },
  play: async ({ canvasElement }) => {
    await checkForStatusElements({
      displayed: [TestIds.renderContent],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    await userEvent.click(within(canvasElement).getByTestId('enable-query'));

    await checkForStatusElements({
      displayed: [StatusTestIds.spinner],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    await waitForLoading();

    await checkForStatusElements({
      displayed: [TestIds.renderContent],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    // make sure that the data is rendered as expected
    await expect(
      within(canvasElement).getByText(JSON.stringify(demoData))
    ).toBeInTheDocument();
  },
};
export const IdleQueryCustomRender: typeof Basic = {
  name: 'Idle Query - Show Custom Idle Render Until Enabled',
  render: () => {
    const [enabled, setEnabled] = useState(false);
    const queryReturn = useQuery<typeof demoData, APIError>({
      queryKey: ['foo'],
      queryFn: () =>
        new Promise(res => {
          setTimeout(() => {
            return res(demoData);
          }, LOADING_TIME);
        }),
      enabled,
    });

    return (
      <div>
        <div className="p-4 mb-4 border-b font-semibold">
          Status: {queryReturn.status}
          <br />
          <Button
            data-testid="enable-query"
            onClick={() => setEnabled(e => !e)}
          >
            Toggle Enabled
          </Button>
        </div>
        <ReactQueryUIWrapper
          renderIdle={() => (
            <div data-testid="awesome-idle">
              <div>This is my custom idle UI!</div>
            </div>
          )}
          useQueryResult={queryReturn}
          render={({ data: result }) => (
            <div>
              <div>
                <div>{shorterTextContent}</div>
              </div>
              <div>Data:</div>
              <div>{JSON.stringify(result)}</div>
            </div>
          )}
        />
      </div>
    );
  },
  play: async ({ canvasElement }) => {
    await expect(
      within(canvasElement).getByTestId('awesome-idle')
    ).toBeInTheDocument();

    await checkForStatusElements({
      displayed: [StatusTestIds.customIdle],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    await userEvent.click(within(canvasElement).getByTestId('enable-query'));

    await checkForStatusElements({
      displayed: [StatusTestIds.spinner],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    await waitForLoading();

    await checkForStatusElements({
      displayed: [TestIds.renderContent],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    // make sure that the data is rendered as expected
    await expect(
      within(canvasElement).getByText(JSON.stringify(demoData))
    ).toBeInTheDocument();
  },
};
export const IdleQueryError: typeof Basic = {
  name: 'Idle Query - No Fallback Data or Custom Render Provided',
  render: () => {
    const [enabled, setEnabled] = useState(false);
    const queryReturn = useQuery<typeof demoData, APIError>({
      queryKey: ['foo'],
      queryFn: () =>
        new Promise(res => {
          setTimeout(() => {
            return res(demoData);
          }, LOADING_TIME);
        }),
      enabled,
    });

    return (
      <div>
        <div className="p-4 mb-4 border-b font-semibold">
          Status: {queryReturn.status}
          <br />
          <Button
            data-testid="enable-query"
            onClick={() => setEnabled(e => !e)}
          >
            Toggle Enabled
          </Button>
        </div>
        <ReactQueryUIWrapper
          useQueryResult={queryReturn}
          render={({ data: result }) => (
            <div>
              <div>
                <div>{shorterTextContent}</div>
              </div>
              <div>Data:</div>
              <div>{JSON.stringify(result)}</div>
            </div>
          )}
        />
      </div>
    );
  },
  play: async ({ canvasElement }) => {
    await checkForStatusElements({
      displayed: [StatusTestIds.error],
      canvasElement,
      childContentId: TestIds.renderContent,
    });

    await expect(
      within(canvasElement).getByText(IdleQueryErrorMessage)
    ).toBeInTheDocument();
  },
};
