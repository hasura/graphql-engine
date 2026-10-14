/**
 * Regression coverage for JsonEditor's prop->local-state sync, which was moved
 * out of a `useEffect` (set-state-in-effect) into the React "adjust state when a
 * prop changes" during-render pattern. The editor keeps a local editing buffer
 * (`localValue`) that must (a) initialise from the `value` prop, (b) reset when
 * `value` changes upstream, (c) NOT clobber in-progress local edits, and
 * (d) never loop.
 *
 * The real AceEditor pulls in ace-builds; we mock it down to a textarea so the
 * component's own state logic runs for real.
 */
import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import { vi } from 'vitest';

vi.mock('@hasura/shared/ui', () => ({
  AceEditor: ({
    value,
    onChange,
    name,
  }: {
    value: string;
    onChange: (v: string) => void;
    name: string;
  }) => (
    <textarea
      data-testid={name}
      value={value}
      onChange={(e) => onChange(e.target.value)}
    />
  ),
}));

import JsonEditor from './JsonEditor';

const editor = () => screen.getByTestId('json-editor') as HTMLTextAreaElement;
const noop = () => undefined;

describe('JsonEditor prop/local-state sync', () => {
  it('renders the initial value prop', () => {
    render(<JsonEditor value='{"a":1}' onChange={noop} />);
    expect(editor().value).toBe('{"a":1}');
  });

  it('resets the local buffer when the value prop changes upstream', () => {
    const { rerender } = render(<JsonEditor value='{"a":1}' onChange={noop} />);
    rerender(<JsonEditor value='{"b":2}' onChange={noop} />);
    expect(editor().value).toBe('{"b":2}');
  });

  it('does not clobber an in-progress local edit (no effect reset loop)', () => {
    render(<JsonEditor value='{"a":1}' onChange={noop} />);
    fireEvent.change(editor(), { target: { value: '{"a":1,"typed":true}' } });
    expect(editor().value).toBe('{"a":1,"typed":true}');
  });

  it('does not re-render unboundedly for a stable value prop', () => {
    let commits = 0;
    const Probe = ({ value }: { value: string }) => {
      commits++;
      return <JsonEditor value={value} onChange={noop} />;
    };
    const { rerender } = render(<Probe value='{"a":1}' />);
    const mountCommits = commits;
    // Re-render the parent with the SAME prop value a few times.
    rerender(<Probe value='{"a":1}' />);
    rerender(<Probe value='{"a":1}' />);
    // Each parent re-render should cause at most a bounded amount of work; the
    // during-render guard must not keep setting state (which would loop).
    expect(commits - mountCommits).toBeLessThanOrEqual(2);
    expect(editor().value).toBe('{"a":1}');
  });
});
