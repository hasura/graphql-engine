import { render, screen } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import userEvent from '@testing-library/user-event';
import { getLSItem } from '@hasura/shared/utils';
import { QuickAdd } from './QuickAdd';
import { parseQueryString } from './utils';

class ResizeObserverStub {
  observe() {
    /* no-op */
  }
  unobserve() {
    /* no-op */
  }
  disconnect() {
    /* no-op */
  }
}
globalThis.ResizeObserver =
  globalThis.ResizeObserver ?? (ResizeObserverStub as never);

// Regression for the set-state-in-effect -> lazy initial state fix. The stored
// GraphiQL queries must be read from localStorage once on mount (not via an
// effect that re-renders), appear alongside the built-in quick operations, and
// not be re-read in a loop.

vi.mock('@hasura/shared/utils', async (orig) => {
  const actual = await orig<Record<string, unknown>>();
  return { ...actual, getLSItem: vi.fn() };
});

vi.mock('./utils', async (orig) => {
  const actual = await orig<Record<string, unknown>>();
  return { ...actual, parseQueryString: vi.fn() };
});

describe('QuickAdd', () => {
  beforeEach(() => {
    vi.mocked(getLSItem).mockReset();
    vi.mocked(parseQueryString).mockReset();
  });

  it('reads localStorage once on mount (lazy init, no render loop)', () => {
    vi.mocked(getLSItem).mockReturnValue(null as any);
    render(
      <Theme>
        <QuickAdd onAdd={vi.fn()} />
      </Theme>,
    );
    expect(screen.getByText('Quick Add')).toBeInTheDocument();
    expect(vi.mocked(getLSItem)).toHaveBeenCalledTimes(1);
  });

  it('includes stored GraphiQL queries next to the built-in operations', async () => {
    vi.mocked(getLSItem).mockReturnValue('{"query":"..."}' as any);
    vi.mocked(parseQueryString).mockReturnValue([
      { name: 'Stored query', query: 'query Stored { id }' },
    ]);

    const user = userEvent.setup();
    render(
      <Theme>
        <QuickAdd onAdd={vi.fn()} />
      </Theme>,
    );

    await user.click(screen.getByText('Quick Add'));

    expect(screen.getByText('Introspection query')).toBeInTheDocument();
    expect(screen.getByText('Stored query')).toBeInTheDocument();
    // still only read once despite the extra render from opening the menu
    expect(vi.mocked(getLSItem)).toHaveBeenCalledTimes(1);
  });

  it('calls onAdd with the selected operation', async () => {
    vi.mocked(getLSItem).mockReturnValue(null as any);
    const onAdd = vi.fn();
    const user = userEvent.setup();
    render(
      <Theme>
        <QuickAdd onAdd={onAdd} />
      </Theme>,
    );

    await user.click(screen.getByText('Quick Add'));
    await user.click(screen.getByText('Introspection query'));

    expect(onAdd).toHaveBeenCalledTimes(1);
    expect(onAdd.mock.calls[0][0].name).toBe('Introspection query');
  });
});
