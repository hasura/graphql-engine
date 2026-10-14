// @vitest-environment jsdom
import { render, cleanup } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { Panel } from './Panel';

const updateGlobals = vi.fn();
let globals: Record<string, unknown> = {};
let params: Record<string, unknown> = {};

vi.mock('storybook/manager-api', () => ({
  useGlobals: () => [globals, updateGlobals],
  useGlobalTypes: () => ({
    adminSecretSet: { control: 'boolean', defaultValue: false },
    consoleType: { control: 'text', defaultValue: 'oss' },
  }),
  useParameter: (key: string) => params[key] ?? null,
}));

vi.mock('storybook/internal/components', () => ({
  AddonPanel: ({ children }: { children: React.ReactNode }) => (
    <div>{children}</div>
  ),
}));

vi.mock('@storybook/addon-docs/blocks', () => ({
  PureArgsTable: () => <div data-testid="args-table" />,
}));

describe('Panel', () => {
  beforeEach(() => {
    vi.useFakeTimers();
    // refreshAndUpdateGlobal schedules an iframe reload; return a stub whose
    // contentDocument is null so the optional-chained reload is a safe no-op.
    vi.spyOn(document, 'getElementById').mockReturnValue({
      contentDocument: null,
    } as unknown as HTMLElement);
  });

  afterEach(() => {
    vi.runOnlyPendingTimers();
    vi.useRealTimers();
    cleanup();
    vi.restoreAllMocks();
    updateGlobals.mockClear();
    globals = {};
    params = {};
  });

  it('pushes a changed story parameter into the globals on mount', () => {
    globals = { adminSecretSet: false, consoleType: 'oss' };
    params = { adminSecretSet: true, consoleType: 'oss' };

    render(<Panel />);

    expect(updateGlobals).toHaveBeenCalledTimes(1);
    expect(updateGlobals).toHaveBeenCalledWith(
      expect.objectContaining({ adminSecretSet: true, consoleType: 'oss' }),
    );
  });

  it('does not touch globals when the parameters already match', () => {
    globals = { adminSecretSet: true, consoleType: 'oss' };
    params = { adminSecretSet: true, consoleType: 'oss' };

    render(<Panel />);

    expect(updateGlobals).not.toHaveBeenCalled();
  });

  it('does not re-sync (revert) when globals change but the parameters do not', () => {
    globals = { adminSecretSet: false, consoleType: 'oss' };
    params = { adminSecretSet: true, consoleType: 'oss' };

    const { rerender } = render(<Panel />);
    expect(updateGlobals).toHaveBeenCalledTimes(1);
    updateGlobals.mockClear();

    // simulate a manual globals edit (e.g. via the args table) that differs
    // from the story parameter; the effect must NOT run again and revert it
    globals = { adminSecretSet: false, consoleType: 'oss' };
    rerender(<Panel />);

    expect(updateGlobals).not.toHaveBeenCalled();
  });
});
