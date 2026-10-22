import { IconButton } from '@hasura/shared/ui';
import { ChangeEventHandler, useState } from 'react';
import { FaCalendar } from 'react-icons/fa';
import DatePicker, { CalendarContainer } from 'react-datepicker';
import { add, isValid, parseISO, sub } from 'date-fns';
import { CustomEventHandler, TextInput, TextInputProps } from './TextInput';

const CustomPickerContainer: React.FC<{
  className: string;
  children?: React.ReactNode;
}> = ({ className, children }) => (
  <div className="absolute top-10 left-0 z-50 min-w-[300px]">
    <CalendarContainer className={className}>
      <div style={{ position: 'relative' }}>{children}</div>
    </CalendarContainer>
  </div>
);

type DateTimeInputProps = {
  formatDate?: (date: Date) => string;
} & TextInputProps;

export const DateTimeInput: React.FC<DateTimeInputProps> = ({
  name,
  disabled,
  placeholder,
  ref,
  onChange,
  onInput,
  onBlur,
  formatDate,
  defaultValue,
  ...rest
}) => {
  const [isCalendarPickerVisible, setCalendarPickerVisible] = useState(false);

  const [selectedDate, setSelectedDate] = useState<Date | null>(null);

  const onDateTimeChange = (date: Date | null) => {
    if (!date) {
      return;
    }

    setSelectedDate(date);
    const timeZoneOffsetMinutes = date.getTimezoneOffset();
    const utcDate = sub(date, { minutes: timeZoneOffsetMinutes });
    const dateString = formatDate ? formatDate(date) : utcDate.toISOString();

    if (ref && 'current' in ref && ref.current) {
      (ref.current as HTMLInputElement).value = dateString;
    }
    if (onChange) {
      const changeCb = onChange as CustomEventHandler;
      changeCb({ target: { value: dateString } });
    }
    if (onInput) {
      const inputCb = onInput as CustomEventHandler;
      inputCb({ target: { value: dateString } });
    }
  };

  return (
    <div className="w-full relative">
      <TextInput
        {...rest}
        className="w-full"
        name={name}
        type="text"
        placeholder={placeholder}
        onChange={onChange as ChangeEventHandler<HTMLInputElement>}
        onInput={onInput as ChangeEventHandler<HTMLInputElement>}
        onBlur={onBlur}
        ref={ref}
        defaultValue={defaultValue}
        rightButton={
          <IconButton
            mode="primary"
            variant="outline"
            disabled={disabled}
            onClick={() => {
              if (disabled) {
                return;
              }
              // Read the current input on open, including any manual edits.
              // Do not substitute the current time for the row's timestamp.
              const value =
                ref && 'current' in ref && ref.current
                  ? (ref.current as HTMLInputElement).value
                  : defaultValue;
              const date =
                typeof value === 'string'
                  ? parseISO(value)
                  : typeof value === 'number'
                    ? new Date(value)
                    : null;
              // Without formatDate, onDateTimeChange writes the picker's wall
              // clock as UTC, so show the stored UTC wall clock to match.
              setSelectedDate(
                date && isValid(date)
                  ? formatDate
                    ? date
                    : add(date, { minutes: date.getTimezoneOffset() })
                  : null,
              );
              setCalendarPickerVisible(!isCalendarPickerVisible);
            }}
          >
            <FaCalendar />
          </IconButton>
        }
        disabled={disabled}
      />
      {isCalendarPickerVisible && (
        <DatePicker
          inline
          showTimeSelect
          selected={selectedDate}
          onClickOutside={() => setCalendarPickerVisible(false)}
          onChange={onDateTimeChange}
          calendarContainer={CustomPickerContainer}
        />
      )}
    </div>
  );
};
