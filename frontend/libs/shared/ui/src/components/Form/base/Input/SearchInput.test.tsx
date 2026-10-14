import '@testing-library/jest-dom/vitest';
import { act, fireEvent, render, screen } from '@testing-library/react';
import { vi } from 'vitest';
import { SearchInput } from './SearchInput';

describe('SearchInput', () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });

  afterEach(() => {
    vi.runOnlyPendingTimers();
    vi.useRealTimers();
  });

  it('renders the controlled value', () => {
    render(<SearchInput onSearch={vi.fn()} value="hello" />);

    expect(screen.getByRole('textbox')).toHaveValue('hello');
  });

  it('calls onSearch after the debounce delay when the user types', () => {
    const onSearch = vi.fn();
    render(<SearchInput onSearch={onSearch} delay={500} />);

    fireEvent.change(screen.getByRole('textbox'), {
      target: { value: 'abc' },
    });

    expect(onSearch).not.toHaveBeenCalled();

    act(() => {
      vi.advanceTimersByTime(500);
    });

    expect(onSearch).toHaveBeenCalledTimes(1);
    expect(onSearch).toHaveBeenCalledWith('abc');
  });

  it('reflects external `value` prop changes without a runaway re-render', () => {
    const onSearch = vi.fn();
    const { rerender } = render(
      <SearchInput onSearch={onSearch} value="a" delay={500} />,
    );

    expect(screen.getByRole('textbox')).toHaveValue('a');

    rerender(<SearchInput onSearch={onSearch} value="b" delay={500} />);
    expect(screen.getByRole('textbox')).toHaveValue('b');

    rerender(<SearchInput onSearch={onSearch} value="c" delay={500} />);
    expect(screen.getByRole('textbox')).toHaveValue('c');

    // External value changes alone must not trigger the search callback.
    act(() => {
      vi.advanceTimersByTime(500);
    });
    expect(onSearch).not.toHaveBeenCalled();
  });
});
