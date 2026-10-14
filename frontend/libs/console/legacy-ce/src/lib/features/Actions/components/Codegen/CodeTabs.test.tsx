import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { Action } from '@hasura/shared/types';
import CodeTabs from './CodeTabs';
import { getFrameworkCodegen } from './utils';

vi.mock('./utils', () => ({
  getFrameworkCodegen: vi.fn(),
}));

// Stub the heavy Ace editor; the rest of the shared UI (Tabs, SkeletonList,
// IndicatorCard, Link) is exercised for real.
vi.mock('@hasura/shared/ui', async (importActual) => {
  const actual = await importActual<Record<string, unknown>>();
  return {
    ...actual,
    AceEditor: ({ value }: { value?: string }) => (
      <div data-testid="ace-editor">{value}</div>
    ),
  };
});

const mockedGetFrameworkCodegen = vi.mocked(getFrameworkCodegen);

const currentAction = { name: 'insertUser' } as Action;

const buildUi = (
  props: Partial<React.ComponentProps<typeof CodeTabs>> = {},
) => (
  <CodeTabs
    framework="nodejs-express"
    actionsSdl="type Mutation { insertUser: User }"
    currentAction={currentAction}
    parentMutation={null}
    shouldDerive={false}
    {...props}
  />
);

const renderCodeTabs = (
  props: Partial<React.ComponentProps<typeof CodeTabs>> = {},
) => {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false }, mutations: { retry: false } },
  });
  const utils = render(
    <QueryClientProvider client={queryClient}>
      <Theme>{buildUi(props)}</Theme>
    </QueryClientProvider>,
  );
  const rerenderWith = (
    next: Partial<React.ComponentProps<typeof CodeTabs>> = {},
  ) =>
    utils.rerender(
      <QueryClientProvider client={queryClient}>
        <Theme>{buildUi(next)}</Theme>
      </QueryClientProvider>,
    );
  return { ...utils, rerenderWith };
};

describe('CodeTabs', () => {
  beforeEach(() => {
    mockedGetFrameworkCodegen.mockReset();
  });

  it('renders the generated files once the codegen request resolves', async () => {
    mockedGetFrameworkCodegen.mockResolvedValue([
      { name: 'actions.js', content: 'console.log(1)' },
    ]);

    renderCodeTabs();

    expect(
      await screen.findAllByText('actions.js', {}, { timeout: 3000 }),
    ).not.toHaveLength(0);
    expect(mockedGetFrameworkCodegen).toHaveBeenCalledWith(
      'nodejs-express',
      'insertUser',
      'type Mutation { insertUser: User }',
      null,
    );
  });

  it('shows an error with a retry action and refetches on retry', async () => {
    mockedGetFrameworkCodegen.mockRejectedValueOnce(new Error('boom'));

    renderCodeTabs();

    expect(
      await screen.findByText(/Error generating code/, {}, { timeout: 3000 }),
    ).toBeInTheDocument();

    mockedGetFrameworkCodegen.mockResolvedValueOnce([
      { name: 'retry.js', content: 'ok' },
    ]);

    fireEvent.click(screen.getByText('Try again'));

    expect(
      await screen.findAllByText('retry.js', {}, { timeout: 3000 }),
    ).not.toHaveLength(0);
  });

  it('does not fetch when no framework is selected', () => {
    renderCodeTabs({ framework: '' });
    expect(mockedGetFrameworkCodegen).not.toHaveBeenCalled();
    expect(screen.queryByText(/Error generating code/)).not.toBeInTheDocument();
  });

  it('regenerates when an input (actionsSdl) changes and does not loop', async () => {
    mockedGetFrameworkCodegen.mockResolvedValue([
      { name: 'file.js', content: 'x' },
    ]);

    const { rerenderWith } = renderCodeTabs();
    await screen.findAllByText('file.js', {}, { timeout: 3000 });
    expect(mockedGetFrameworkCodegen).toHaveBeenCalledTimes(1);

    rerenderWith({ actionsSdl: 'type Mutation { insertUser: User! }' });

    await waitFor(() => {
      expect(mockedGetFrameworkCodegen).toHaveBeenCalledTimes(2);
    });

    // give any runaway re-render a chance to fire extra requests
    await new Promise((r) => setTimeout(r, 50));
    expect(mockedGetFrameworkCodegen).toHaveBeenCalledTimes(2);
  });
});
