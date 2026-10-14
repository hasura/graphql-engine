import { renderHook, act, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import useActionForm from './useActionForm';
import getDefaultState, {
  getActionRequestTransformDefaultState,
  getActionResponseTransformDefaultState,
} from './state';

const resolvers: Array<(value: Record<string, any>) => void> = [];
const testWebhookTransform = vi.fn(
  () =>
    new Promise<Record<string, any>>((resolve) => {
      resolvers.push(resolve);
    }),
);

vi.mock('@hasura/shared/context', async (importActual) => {
  const actual = await importActual<Record<string, unknown>>();
  return { ...actual, useAppContext: () => ({ readOnlyMode: false }) };
});

vi.mock('../../../ConfigureTransformation', async (importActual) => {
  const actual = await importActual<Record<string, unknown>>();
  return { ...actual, useTestWebhookTransform: () => testWebhookTransform };
});

const wrapper = ({ children }: { children: React.ReactNode }) => {
  const client = new QueryClient({
    defaultOptions: { queries: { retry: false }, mutations: { retry: false } },
  });
  return <QueryClientProvider client={client}>{children}</QueryClientProvider>;
};

const renderActionForm = () =>
  renderHook(
    () =>
      useActionForm(
        getDefaultState(null, null),
        getActionRequestTransformDefaultState(),
        getActionResponseTransformDefaultState(),
      ),
    { wrapper },
  );

const flushAll = () =>
  act(async () => {
    resolvers.splice(0).forEach((resolve) => resolve({}));
    await Promise.resolve();
  });

describe('useActionForm', () => {
  beforeEach(() => {
    resolvers.length = 0;
    testWebhookTransform.mockClear();
  });

  it('is not fetching while no handler is configured', async () => {
    const { result } = renderActionForm();
    expect(result.current.isFetching).toBe(false);
    expect(testWebhookTransform).not.toHaveBeenCalled();
    await flushAll();
  });

  it('marks isFetching while a webhook transform request is in flight and clears it after', async () => {
    const { result } = renderActionForm();

    act(() => {
      result.current.handlerOnChange('http://handler.test');
    });

    // the transform preview request fires and isFetching flips on
    await waitFor(() => {
      expect(testWebhookTransform).toHaveBeenCalled();
    });
    await waitFor(() => {
      expect(result.current.isFetching).toBe(true);
    });

    await flushAll();

    await waitFor(() => {
      expect(result.current.isFetching).toBe(false);
    });
  });

  it('does not fire the preview request in a runaway loop', async () => {
    const { result } = renderActionForm();

    act(() => {
      result.current.handlerOnChange('http://handler.test');
    });

    await waitFor(() => {
      expect(testWebhookTransform).toHaveBeenCalled();
    });
    await flushAll();
    await waitFor(() => {
      expect(result.current.isFetching).toBe(false);
    });

    const callsAfterSettle = testWebhookTransform.mock.calls.length;
    await new Promise((r) => setTimeout(r, 50));
    expect(testWebhookTransform.mock.calls.length).toBe(callsAfterSettle);
  });
});
