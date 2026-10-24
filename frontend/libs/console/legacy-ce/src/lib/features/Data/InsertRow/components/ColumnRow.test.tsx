import { StrictMode } from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { vi } from 'vitest';
import { Theme } from '@radix-ui/themes';
import { ColumnRow, ColumnRowProps } from './ColumnRow';

const baseProps = (
  overrides: Partial<ColumnRowProps> = {},
): ColumnRowProps => ({
  isDisabled: false,
  isNullDisabled: false,
  isDefaultDisabled: false,
  label: 'id',
  name: 'id',
  dataType: 'integer',
  consoleDataType: 'integer',
  onChange: vi.fn(),
  resetToken: '',
  placeholder: 'placeholder...',
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

describe('ColumnRow initial values', () => {
  it.each([
    ['uuid', 'uuid', '31b789b0-acfe-43a8-bdaf-561313e8f82e'],
    ['float8', 'float', 1.5],
    ['integer', 'integer', 0],
    ['date', 'date', '2026-10-22'],
    ['timestamp with time zone', 'timestamp', '2026-10-22T10:48:04+00:00'],
    ['time without time zone', 'time', '10:48:04'],
    ['custom_scalar', 'string', '123-custom'],
    ['text', 'text', 'x'],
  ] as const)(
    'keeps the existing %s value on mount',
    (dataType, consoleDataType, initialValue) => {
      const onChange = vi.fn();
      render(
        <ColumnRow
          {...baseProps({ dataType, consoleDataType, initialValue, onChange })}
        />,
      );
      expect(screen.getByRole('textbox')).toHaveValue(String(initialValue));
      expect(screen.getByRole('radio', { name: 'id' })).toBeChecked();
      expect(onChange).not.toHaveBeenCalled();
    },
  );

  it('does not clear initial values when effects are replayed in StrictMode', () => {
    const onChange = vi.fn();
    render(
      <StrictMode>
        <ColumnRow
          {...baseProps({
            dataType: 'uuid',
            consoleDataType: 'uuid',
            initialValue: '31b789b0-acfe-43a8-bdaf-561313e8f82e',
            onChange,
          })}
        />
      </StrictMode>,
    );
    expect(screen.getByRole('textbox')).toHaveValue(
      '31b789b0-acfe-43a8-bdaf-561313e8f82e',
    );
    expect(onChange).not.toHaveBeenCalled();
  });
});

describe('ColumnRow enum column', () => {
  beforeAll(() => {
    // Radix Select measures with ResizeObserver and opens its listbox with the
    // Pointer Capture / scrollIntoView DOM APIs, which jsdom does not implement.
    global.ResizeObserver ??= class {
      observe() {
        /* noop */
      }
      unobserve() {
        /* noop */
      }
      disconnect() {
        /* noop */
      }
    } as unknown as typeof ResizeObserver;
    if (!Element.prototype.hasPointerCapture) {
      Element.prototype.hasPointerCapture = () => false;
    }
    if (!Element.prototype.setPointerCapture) {
      Element.prototype.setPointerCapture = () => undefined;
    }
    if (!Element.prototype.releasePointerCapture) {
      Element.prototype.releasePointerCapture = () => undefined;
    }
    if (!Element.prototype.scrollIntoView) {
      Element.prototype.scrollIntoView = () => undefined;
    }
  });

  const enumProps = (overrides: Partial<ColumnRowProps> = {}) =>
    baseProps({
      label: 'status',
      name: 'status',
      dataType: 'text',
      consoleDataType: 'text',
      enumValues: ['active', 'archived'],
      ...overrides,
    });

  it('renders a select of the enum values instead of a text input', () => {
    render(<ColumnRow {...enumProps({ initialValue: 'archived' })} />, {
      wrapper: Theme,
    });

    expect(screen.queryByRole('textbox')).not.toBeInTheDocument();
    expect(screen.getByRole('combobox')).toHaveTextContent('archived');
  });

  it('reports the selected value and selects the value radio', async () => {
    const onChange = vi.fn();
    render(<ColumnRow {...enumProps({ onChange })} />, { wrapper: Theme });

    await userEvent.click(screen.getByRole('combobox'));
    await userEvent.click(
      await screen.findByRole('option', { name: 'active' }),
    );

    expect(onChange).toHaveBeenCalledWith({
      columnName: 'status',
      selectionType: 'value',
      value: 'active',
    });
    expect(screen.getByRole('radio', { name: 'status' })).toBeChecked();
    expect(screen.getByRole('combobox')).toHaveTextContent('active');
  });

  it('clears the selection on reset', async () => {
    const onChange = vi.fn();
    const props = enumProps({ onChange });
    const { rerender } = render(<ColumnRow {...props} />, {
      wrapper: Theme,
    });

    await userEvent.click(screen.getByRole('combobox'));
    await userEvent.click(
      await screen.findByRole('option', { name: 'active' }),
    );
    onChange.mockClear();

    rerender(<ColumnRow {...props} resetToken="reset-1" />);

    expect(onChange).toHaveBeenCalledWith({
      columnName: 'status',
      selectionType: 'default',
      value: '',
    });
    expect(screen.getByRole('combobox')).not.toHaveTextContent('active');
  });
});

describe('ColumnRow array column', () => {
  const arrayProps = (overrides: Partial<ColumnRowProps> = {}) =>
    baseProps({
      label: 'scores',
      name: 'scores',
      dataType: 'integer[]',
      consoleDataType: 'array',
      arrayItemType: 'integer',
      ...overrides,
    });

  const items = () =>
    screen.getAllByRole('textbox', {
      name: /scores item/,
    }) as HTMLInputElement[];

  it('renders an input per item of the existing value', () => {
    render(<ColumnRow {...arrayProps({ initialValue: '{1,2}' })} />, {
      wrapper: Theme,
    });

    expect(items().map((input) => input.value)).toEqual(['1', '2']);
    expect(screen.getByRole('radio', { name: 'scores' })).toBeChecked();
  });

  it('reports the typed items as the array value', async () => {
    const onChange = vi.fn();
    render(<ColumnRow {...arrayProps({ initialValue: [1], onChange })} />, {
      wrapper: Theme,
    });

    await userEvent.click(screen.getByRole('button', { name: 'Add item' }));
    await userEvent.type(items()[1], '3');
    await userEvent.click(screen.getByRole('button', { name: 'Add item' }));

    // an empty integer item is a NULL element
    expect(onChange).toHaveBeenLastCalledWith({
      columnName: 'scores',
      selectionType: 'value',
      value: [1, 3, null],
    });
    expect(screen.getByRole('radio', { name: 'scores' })).toBeChecked();

    await userEvent.click(
      screen.getByRole('button', { name: 'Remove scores item 1' }),
    );
    expect(onChange).toHaveBeenLastCalledWith({
      columnName: 'scores',
      selectionType: 'value',
      value: [3, null],
    });
  });

  it('clears the items on reset', async () => {
    const onChange = vi.fn();
    const props = arrayProps({ initialValue: ['a'], onChange });
    const { rerender } = render(<ColumnRow {...props} />, { wrapper: Theme });

    rerender(<ColumnRow {...props} resetToken="reset-1" />);

    expect(screen.queryAllByRole('textbox', { name: /scores item/ })).toEqual(
      [],
    );
    expect(onChange).toHaveBeenCalledWith({
      columnName: 'scores',
      selectionType: 'default',
      value: '',
    });
  });
});
