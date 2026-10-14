import '@testing-library/jest-dom/vitest';
import { fireEvent, render, screen } from '@testing-library/react';
import { vi } from 'vitest';
import { JSONEditor } from './index';

// Replace the heavy Ace editor with a lightweight controlled textarea so we can
// exercise JSONEditor's state/sync logic deterministically.
vi.mock('../AceEditor', () => ({
  AceEditor: ({
    value,
    onChange,
  }: {
    value: string;
    onChange: (v: string) => void;
  }) => (
    <textarea
      data-testid="editor"
      value={value}
      onChange={(e) => onChange(e.target.value)}
    />
  ),
}));

describe('JSONEditor', () => {
  it('renders the initial data', () => {
    render(<JSONEditor data='{"a":1}' onChange={vi.fn()} />);

    expect(screen.getByTestId('editor')).toHaveValue('{"a":1}');
  });

  it('notifies the parent when the editor value changes', () => {
    const onChange = vi.fn();
    render(<JSONEditor data='{"a":1}' onChange={onChange} />);

    fireEvent.change(screen.getByTestId('editor'), {
      target: { value: '{"a":2}' },
    });

    expect(screen.getByTestId('editor')).toHaveValue('{"a":2}');
    expect(onChange).toHaveBeenCalledWith('{"a":2}');
  });

  it('syncs the editor when the `data` prop changes to a valid json string', () => {
    const { rerender } = render(
      <JSONEditor data='{"a":1}' onChange={vi.fn()} />,
    );

    expect(screen.getByTestId('editor')).toHaveValue('{"a":1}');

    rerender(<JSONEditor data='{"b":2}' onChange={vi.fn()} />);

    expect(screen.getByTestId('editor')).toHaveValue('{"b":2}');
  });

  it('uses the latest onChange without a runaway re-render', () => {
    const first = vi.fn();
    const { rerender } = render(<JSONEditor data='{"a":1}' onChange={first} />);

    const second = vi.fn();
    rerender(<JSONEditor data='{"a":1}' onChange={second} />);

    fireEvent.change(screen.getByTestId('editor'), {
      target: { value: '{"a":9}' },
    });

    expect(first).not.toHaveBeenCalled();
    expect(second).toHaveBeenCalledWith('{"a":9}');
    expect(second).toHaveBeenCalledTimes(1);
  });
});
