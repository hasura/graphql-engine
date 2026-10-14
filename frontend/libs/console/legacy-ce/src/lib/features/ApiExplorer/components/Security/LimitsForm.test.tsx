/**
 * Regression coverage for LimitsForm's `addUniqueParams`, which is both derived
 * from props AND user-toggleable. The sync was moved out of a `useEffect`
 * (set-state-in-effect) into a lazy initial value + the React "adjust state when
 * a prop changes" during-render pattern. Covers: initial value from props,
 * re-sync when the source props change, that a user toggle is NOT immediately
 * overwritten, and no runaway renders.
 */
import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { vi } from 'vitest';
import { LimitsForm } from './LimitsForm';
import { RoleState } from './utils';

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

type Props = React.ComponentProps<typeof LimitsForm>;

const baseProps = (overrides: Partial<Props> = {}): Props => ({
  limit: 'rate_limit',
  label: { title: 'Rate Limit', info: <span>info</span> },
  state: RoleState.enabled,
  globalLimit: 10,
  role: 'user',
  unique_params_role: null,
  max_reqs_role: 5,
  onRadioChange: () => vi.fn(),
  onUniqueParamsChange: () => vi.fn(),
  onInputChange: () => vi.fn(),
  ...overrides,
});

const renderForm = (overrides: Partial<Props> = {}) => {
  const props = baseProps(overrides);
  const node = (p: Props) => (
    <Theme>
      <LimitsForm {...p} />
    </Theme>
  );
  const utils = render(node(props));
  return {
    ...utils,
    rerenderWith: (patch: Partial<Props>) =>
      utils.rerender(node({ ...props, ...patch })),
  };
};

const checkbox = () => screen.getByRole('checkbox') as HTMLElement;
const isChecked = () => checkbox().getAttribute('aria-checked') === 'true';

describe('LimitsForm addUniqueParams sync', () => {
  it('is OFF initially when there are no unique params', () => {
    renderForm({ unique_params_role: null });
    expect(isChecked()).toBe(false);
  });

  it('is ON initially when unique params are present (seeded from props on mount)', () => {
    renderForm({ unique_params_role: ['x-hasura-user-id'] });
    expect(isChecked()).toBe(true);
  });

  it('re-syncs when the source props change', () => {
    const { rerenderWith } = renderForm({
      unique_params_role: ['x-hasura-user-id'],
    });
    expect(isChecked()).toBe(true);
    // switch to a different role that has no unique params
    rerenderWith({ role: 'editor', unique_params_role: null });
    expect(isChecked()).toBe(false);
  });

  it('does not immediately overwrite a user toggle when unrelated props re-render', () => {
    const { rerenderWith } = renderForm({
      unique_params_role: ['x-hasura-user-id'],
    });
    expect(isChecked()).toBe(true);
    // user unchecks
    fireEvent.click(checkbox());
    expect(isChecked()).toBe(false);
    // unrelated re-render with the SAME source props: toggle must persist
    rerenderWith({ max_reqs_role: 9 });
    expect(isChecked()).toBe(false);
  });

  it('does not re-render unboundedly for stable props', () => {
    let commits = 0;
    const Probe = () => {
      commits++;
      return (
        <Theme>
          <LimitsForm {...baseProps({ unique_params_role: ['a'] })} />
        </Theme>
      );
    };
    const { rerender } = render(<Probe />);
    const mountCommits = commits;
    rerender(<Probe />);
    rerender(<Probe />);
    expect(commits - mountCommits).toBeLessThanOrEqual(2);
  });
});
