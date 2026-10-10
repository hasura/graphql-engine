import { render, screen, fireEvent, act } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import HandlerEditor from './HandlerEditor';

const renderEditor = (
  props: Partial<React.ComponentProps<typeof HandlerEditor>> = {},
) => {
  const onChange = vi.fn();
  const utils = render(
    <Theme>
      <HandlerEditor value="" onChange={onChange} disabled={false} {...props} />
    </Theme>,
  );
  return { ...utils, onChange };
};

describe('HandlerEditor', () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    vi.runOnlyPendingTimers();
    vi.useRealTimers();
  });

  it('renders the label and the handler input with the initial value', () => {
    renderEditor({ value: 'http://handler.test/api' });

    expect(screen.getByText('Webhook (HTTP/S) Handler')).toBeInTheDocument();
    expect(screen.getByRole('textbox')).toHaveValue('http://handler.test/api');
  });

  it('updates the input immediately but debounces the onChange callback', () => {
    const { onChange } = renderEditor({ value: '' });

    const input = screen.getByRole('textbox');
    act(() => {
      fireEvent.change(input, { target: { value: 'http://new-handler.test' } });
    });

    // input reflects the typed value right away
    expect(input).toHaveValue('http://new-handler.test');
    // but onChange has not fired yet (still within the debounce window)
    expect(onChange).not.toHaveBeenCalled();

    act(() => {
      vi.advanceTimersByTime(999);
    });
    expect(onChange).not.toHaveBeenCalled();

    act(() => {
      vi.advanceTimersByTime(1);
    });
    expect(onChange).toHaveBeenCalledTimes(1);
    expect(onChange).toHaveBeenCalledWith('http://new-handler.test');
  });

  it('disables the input when disabled is true', () => {
    renderEditor({ disabled: true });
    expect(screen.getByRole('textbox')).toBeDisabled();
  });
});
