import { render, screen, fireEvent } from '@testing-library/react';
import { vi } from 'vitest';
import { ColumnRow, ColumnRowProps } from './ColumnRow';

const baseProps = (
  overrides: Partial<ColumnRowProps> = {},
): ColumnRowProps => ({
  isDisabled: false,
  isNullDisabled: false,
  isDefaultDisabled: false,
  label: 'id',
  name: 'id',
  dataType: 'number',
  onChange: vi.fn(),
  resetToken: '',
  placeholder: 'placeholder...',
  driver: 'postgres',
  supportedDataTypes: undefined,
  ...overrides,
});

describe('ColumnRow reset effect', () => {
  it('renders the value input on initial render', () => {
    render(<ColumnRow {...baseProps()} />);
    expect(screen.getByRole('textbox')).toBeInTheDocument();
  });

  it('clears the input and notifies the parent when resetToken changes', () => {
    const onChange = vi.fn();
    const props = baseProps({ onChange });
    const { rerender } = render(<ColumnRow {...props} />);

    const input = screen.getByRole('textbox') as HTMLInputElement;
    fireEvent.change(input, { target: { value: '42' } });
    expect(input).toHaveValue('42');

    onChange.mockClear();

    // Changing the reset token should run the reset effect exactly once.
    rerender(<ColumnRow {...props} resetToken="reset-1" />);

    expect(input).toHaveValue('');
    expect(onChange).toHaveBeenCalledTimes(1);
    expect(onChange).toHaveBeenCalledWith({
      columnName: 'id',
      selectionType: 'default',
      value: '',
    });
  });

  it('does not run the reset effect when unrelated props change (latest onChange is used, not re-triggered)', () => {
    const onChange = vi.fn();
    const props = baseProps({ onChange });
    const { rerender } = render(<ColumnRow {...props} />);

    const input = screen.getByRole('textbox') as HTMLInputElement;
    fireEvent.change(input, { target: { value: '42' } });

    onChange.mockClear();

    // Re-render with a brand new onChange identity (as a parent that doesn't
    // memoize would do) but the SAME resetToken: the effect must not fire.
    rerender(<ColumnRow {...props} onChange={vi.fn()} placeholder="changed" />);

    expect(input).toHaveValue('42');
    expect(onChange).not.toHaveBeenCalled();
  });

  it('uses the latest onChange when the reset finally happens', () => {
    const firstOnChange = vi.fn();
    const latestOnChange = vi.fn();
    const props = baseProps({ onChange: firstOnChange });
    const { rerender } = render(<ColumnRow {...props} />);

    const input = screen.getByRole('textbox') as HTMLInputElement;
    fireEvent.change(input, { target: { value: '42' } });

    // Swap onChange without changing resetToken (no reset yet) ...
    rerender(<ColumnRow {...props} onChange={latestOnChange} />);
    // ... then change resetToken: the LATEST onChange should be called.
    rerender(
      <ColumnRow {...props} onChange={latestOnChange} resetToken="reset-1" />,
    );

    expect(firstOnChange).not.toHaveBeenCalledWith(
      expect.objectContaining({ selectionType: 'default' }),
    );
    expect(latestOnChange).toHaveBeenCalledWith({
      columnName: 'id',
      selectionType: 'default',
      value: '',
    });
  });
});
