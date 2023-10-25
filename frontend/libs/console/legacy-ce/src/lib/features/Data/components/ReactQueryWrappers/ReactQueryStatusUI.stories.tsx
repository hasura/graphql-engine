import { expect } from '@storybook/jest';
import { Meta, StoryObj } from '@storybook/react';
import { within } from '@storybook/testing-library';
import { useState } from 'react';
import { Button } from '../../../../new-components/Button';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { ReactQueryStatusUI, TestIds } from './ReactQueryStatusUI';
import { checkForStatusElements, shorterTextContent } from './story-utils';
import { ErrorType } from './types';

export default {
  component: ReactQueryStatusUI,
  decorators: [
    (story, { args }) => {
      const [count, setCount] = useState(0);
      return (
        <div>
          <div className="p-4 mb-4 border-b font-semibold">
            Status: {args.status}
          </div>
          {story()}
          <br />
          <div>
            <Button
              data-testid="clicker"
              onClick={() => setCount(prev => prev + 1)}
            >
              For Testing Overlays
            </Button>
            <div>Click Count: {count}</div>
          </div>
        </div>
      );
    },
  ],
} satisfies Meta<typeof ReactQueryStatusUI>;

const defaultError: ErrorType = { name: 'foo', message: 'Error message' };

export const Playground: StoryObj<typeof ReactQueryStatusUI> = {
  args: {
    status: 'loading',
    error: defaultError,
  },
  render: args => <ReactQueryStatusUI {...args} />,
};

export const Success: typeof Playground = {
  args: {
    status: 'success',
    error: defaultError,
  },
  render: args => (
    <ReactQueryStatusUI status={args.status} error={args.error}>
      <div>{shorterTextContent}</div>
    </ReactQueryStatusUI>
  ),
  play: async ({ canvasElement }) => {
    await checkForStatusElements({
      displayed: [TestIds.childContent],
      canvasElement,
    });
  },
};

export const LoadingSpinner: typeof Playground = {
  ...Playground,
  name: 'Loading - Spinner - instead of content (default)',
  render: args => (
    <ReactQueryStatusUI status={args.status} error={args.error}>
      <div>{shorterTextContent}</div>
    </ReactQueryStatusUI>
  ),
  play: async ({ canvasElement }) => {
    await checkForStatusElements({
      displayed: [TestIds.spinner],
      canvasElement,
    });
  },
};

export const LoadingSkeleton: typeof Playground = {
  ...LoadingSpinner,
  name: 'Loading - Skeleton - instead of content',
  render: args => (
    <ReactQueryStatusUI
      status={args.status}
      error={args.error}
      loader="skeleton"
      skeletonProps={{ count: 3, height: 30 }}
    >
      <div>{shorterTextContent}</div>
    </ReactQueryStatusUI>
  ),
  play: async ({ canvasElement }) => {
    await checkForStatusElements({
      displayed: [TestIds.skeleton],
      canvasElement,
    });
  },
};

export const LoadingSkeletonOverley: typeof Playground = {
  ...LoadingSpinner,
  name: 'Loading - Skeleton - overlay',
  render: args => (
    <ReactQueryStatusUI
      status={args.status}
      error={args.error}
      fallbackData={[]}
      loader="skeleton"
      loadingStyle="overlay"
      skeletonCount={3}
    >
      <div>
        <div>{shorterTextContent}</div>
        <div>{shorterTextContent}</div>
        <div>{shorterTextContent}</div>
      </div>
    </ReactQueryStatusUI>
  ),
  play: async ({ canvasElement }) => {
    const c = within(canvasElement);
    await checkForStatusElements({
      displayed: [
        TestIds.skeleton,
        TestIds.loadingOverlay,
        TestIds.childContent,
      ],
      canvasElement,
    });

    await expect(c.getByTestId(TestIds.childContent)).not.toBeVisible();

    // check that the overlay is exactly the same height as the child content:
    const childHeight = c.getByTestId(TestIds.childContent).offsetHeight;
    const overlayHeight = (
      c
        .getByTestId(TestIds.skeleton)
        .querySelector('span.dynamic-skeleton-grid') as HTMLElement
    ).offsetHeight;

    await expect(childHeight).toEqual(overlayHeight);
  },
};

export const SpinnerOver: typeof Playground = {
  ...Playground,
  name: 'Loading - Spinner - overlay',
  render: args => (
    <ReactQueryStatusUI
      status={args.status}
      error={args.error}
      loader="spinner"
      loadingStyle="overlay"
      fallbackData={[]}
    >
      <div>{shorterTextContent}</div>
    </ReactQueryStatusUI>
  ),
  play: async ({ canvasElement }) => {
    await checkForStatusElements({
      displayed: [
        TestIds.spinner,
        TestIds.loadingOverlay,
        TestIds.childContent,
      ],
      canvasElement,
    });
  },
};

export const ErrorInsteadOfChildren: typeof Playground = {
  name: 'Error - instead of children (default)',
  args: {
    ...Playground.args,
    status: 'error',
  },
  render: args => (
    <ReactQueryStatusUI status={args.status} error={args.error}>
      <div>Children</div>
    </ReactQueryStatusUI>
  ),

  play: async ({ canvasElement }) => {
    const c = within(canvasElement);
    await checkForStatusElements({
      displayed: [TestIds.error],
      canvasElement,
    });
    await expect(c.getByText(defaultError.message)).toBeInTheDocument();
  },
};

export const ErrorAboveChildren: typeof Playground = {
  name: 'Error - above children',
  args: {
    ...Playground.args,
    status: 'error',
  },
  render: args => (
    <ReactQueryStatusUI
      status={args.status}
      error={args.error}
      errorStyle="above-content"
      fallbackData={[]}
    >
      <div>Children</div>
    </ReactQueryStatusUI>
  ),
  play: async ({ canvasElement }) => {
    const c = within(canvasElement);
    await checkForStatusElements({
      displayed: [TestIds.error, TestIds.errorAbove, TestIds.childContent],
      canvasElement,
    });
    await expect(c.getByText(defaultError.message)).toBeInTheDocument();
  },
};

export const ErrorBelowChildren: typeof Playground = {
  name: 'Error - below children',
  args: {
    ...Playground.args,
    status: 'error',
  },
  render: args => (
    <ReactQueryStatusUI
      status={args.status}
      error={args.error}
      errorStyle="below-content"
      fallbackData={[]}
    >
      <div>Children</div>
    </ReactQueryStatusUI>
  ),
  play: async ({ canvasElement }) => {
    const c = within(canvasElement);
    await checkForStatusElements({
      displayed: [TestIds.error, TestIds.errorBelow, TestIds.childContent],
      canvasElement,
    });
    await expect(c.getByText(defaultError.message)).toBeInTheDocument();
  },
};

export const CustomError: typeof Playground = {
  name: 'Custom Error - instead of children',
  args: {
    ...Playground.args,
    status: 'error',
  },
  render: args => (
    <ReactQueryStatusUI
      status={args.status}
      error={args.error}
      renderError={({ error }) => (
        <div>
          <div>This is my custom error UI!</div>
          <div>Here's the error message:</div>
          <br />
          <strong>{error?.message}</strong>
        </div>
      )}
    >
      <div>Children</div>
    </ReactQueryStatusUI>
  ),
  play: async ({ canvasElement }) => {
    const c = within(canvasElement);
    await checkForStatusElements({
      displayed: [TestIds.customError],
      canvasElement,
    });

    await expect(c.getByText(defaultError.message)).toBeInTheDocument();
  },
};

export const CustomErrorBelowChildren: typeof Playground = {
  name: 'Custom Error - below children',
  args: {
    ...Playground.args,
    status: 'error',
  },
  render: args => (
    <ReactQueryStatusUI
      status={args.status}
      error={args.error}
      errorStyle="below-content"
      fallbackData={[]}
      renderError={({ error }) => (
        <div className="text-red-600">{error?.message}</div>
      )}
    >
      <div>Children</div>
    </ReactQueryStatusUI>
  ),
  play: async ({ canvasElement }) => {
    const c = within(canvasElement);
    await checkForStatusElements({
      displayed: [
        TestIds.customError,
        TestIds.errorBelow,
        TestIds.childContent,
      ],
      canvasElement,
    });

    await expect(c.getByText(defaultError.message)).toBeInTheDocument();
  },
};

export const CustomLoadingDisplay: typeof Playground = {
  ...Playground,
  name: 'Custom Loading - instead of children',
  render: args => {
    return (
      <ReactQueryStatusUI
        status={args.status}
        error={args.error}
        renderLoading={() => (
          <div data-testid="awesome-loading">
            <div>This is my custom loading UI!</div>
          </div>
        )}
      >
        <div>Children</div>
      </ReactQueryStatusUI>
    );
  },
  play: async ({ canvasElement }) => {
    const c = within(canvasElement);
    await checkForStatusElements({
      displayed: [TestIds.customLoading],
      canvasElement,
    });

    await expect(c.getByTestId('awesome-loading')).toBeInTheDocument();
  },
};

export const CustomLoadingDisplayOverlay: typeof Playground = {
  ...Playground,
  name: 'Custom Loading - instead of children',
  render: args => {
    return (
      <ReactQueryStatusUI
        status={args.status}
        error={args.error}
        loadingStyle="overlay"
        fallbackData={[]}
        renderLoading={() => (
          <div data-testid="awesome-loading">
            <div>This is my custom loading UI!</div>
          </div>
        )}
      >
        <div>Children</div>
      </ReactQueryStatusUI>
    );
  },
  play: async ({ canvasElement }) => {
    const c = within(canvasElement);
    await checkForStatusElements({
      displayed: [
        TestIds.customLoading,
        TestIds.loadingOverlay,
        TestIds.childContent,
      ],
      canvasElement,
    });

    await expect(c.getByTestId('awesome-loading')).toBeInTheDocument();
  },
};

export const CustomIdleDisplay: typeof Playground = {
  args: {
    ...Playground.args,
    status: 'idle',
  },
  render: args => {
    return (
      <ReactQueryStatusUI
        status={args.status}
        error={args.error}
        renderIdle={() => (
          <div data-testid="awesome-idle">
            <div>This is my custom idle UI!</div>
          </div>
        )}
      >
        <div>Children</div>
      </ReactQueryStatusUI>
    );
  },
  play: async ({ canvasElement }) => {
    const c = within(canvasElement);
    await checkForStatusElements({
      displayed: [TestIds.customIdle],
      canvasElement,
    });

    await expect(c.getByTestId('awesome-idle')).toBeInTheDocument();
  },
};
