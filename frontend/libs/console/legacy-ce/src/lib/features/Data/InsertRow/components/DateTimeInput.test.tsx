import { createRef } from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import { DateTimeInput } from './DateTimeInput';

vi.mock('react-datepicker', () => ({
  default: ({
    selected,
    onChange,
  }: {
    selected: Date | null;
    onChange: (date: Date) => void;
  }) => (
    <button
      type="button"
      onClick={() => onChange(new Date('2026-10-23T15:30:00Z'))}
    >
      {selected?.toISOString() ?? 'No date selected'}
    </button>
  ),
  CalendarContainer: ({ children }: { children: React.ReactNode }) => children,
}));

describe('DateTimeInput calendar selection', () => {
  it('opens on the existing row timestamp instead of the current time', () => {
    render(
      <DateTimeInput
        ref={createRef<HTMLInputElement>()}
        defaultValue="2026-10-22T10:48:04+00:00"
      />,
    );
    fireEvent.click(screen.getByRole('button'));
    expect(
      screen.getByRole('button', { name: '2026-10-22T10:48:04.000Z' }),
    ).toBeInTheDocument();
  });

  it('uses manual edits when reopening the picker', () => {
    render(
      <DateTimeInput
        ref={createRef<HTMLInputElement>()}
        defaultValue="2026-10-22T10:48:04+00:00"
      />,
    );
    fireEvent.change(screen.getByRole('textbox'), {
      target: { value: '2026-10-20T12:34:00+07:00' },
    });
    fireEvent.click(screen.getByRole('button'));
    expect(
      screen.getByRole('button', { name: '2026-10-20T05:34:00.000Z' }),
    ).toBeInTheDocument();
  });

  it.each(['', 'invalid'])(
    'does not select an invalid or empty timestamp %j',
    (defaultValue) => {
      render(
        <DateTimeInput
          ref={createRef<HTMLInputElement>()}
          defaultValue={defaultValue}
        />,
      );
      fireEvent.click(screen.getByRole('button'));
      expect(
        screen.getByRole('button', { name: 'No date selected' }),
      ).toBeInTheDocument();
    },
  );

  it.each([
    // Without formatDate the picker's wall clock is written as UTC, so a
    // UTC+7 user must see 10:48 local, i.e. the instant 03:48Z.
    ['without formatDate', undefined, '2026-10-22T03:48:04.000Z'],
    // formatDate writes local time, so the stored instant is shown as is.
    [
      'with formatDate',
      (date: Date) => date.toISOString(),
      '2026-10-22T10:48:04.000Z',
    ],
  ])(
    'opens on the stored wall clock outside UTC %s',
    (_, formatDate, expected) => {
      // UTC+7: getTimezoneOffset() is negative east of UTC.
      const offsetSpy = vi
        .spyOn(Date.prototype, 'getTimezoneOffset')
        .mockReturnValue(-420);
      try {
        render(
          <DateTimeInput
            ref={createRef<HTMLInputElement>()}
            defaultValue="2026-10-22T10:48:04.000Z"
            formatDate={formatDate}
          />,
        );
        fireEvent.click(screen.getByRole('button'));
        expect(
          screen.getByRole('button', { name: expected }),
        ).toBeInTheDocument();
      } finally {
        offsetSpy.mockRestore();
      }
    },
  );

  it('updates the selection and input after choosing a timestamp', () => {
    const onChange = vi.fn();
    render(
      <DateTimeInput
        ref={createRef<HTMLInputElement>()}
        defaultValue="2026-10-22T10:48:04+00:00"
        onChange={onChange}
        formatDate={(date) => date.toISOString()}
      />,
    );
    fireEvent.click(screen.getByRole('button'));
    fireEvent.click(
      screen.getByRole('button', { name: '2026-10-22T10:48:04.000Z' }),
    );
    expect(
      screen.getByDisplayValue('2026-10-23T15:30:00.000Z'),
    ).toBeInTheDocument();
    expect(onChange).toHaveBeenCalledWith({
      target: { value: '2026-10-23T15:30:00.000Z' },
    });
    expect(
      screen.getByRole('button', { name: '2026-10-23T15:30:00.000Z' }),
    ).toBeInTheDocument();
  });
});
