import { IconButton } from '@hasura/shared/ui';
import { ChangeEventHandler, useState } from 'react';
import { FaCalendar } from 'react-icons/fa';
import DatePicker, { CalendarContainer } from 'react-datepicker';
import { sub } from 'date-fns';
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

  const onDateTimeChange = (date: Date | null) => {
    if (!date) {
      return;
    }

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
            disabled={disabled}
            onClick={() => {
              if (disabled) {
                return;
              }
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
          onClickOutside={() => setCalendarPickerVisible(false)}
          onChange={onDateTimeChange}
          calendarContainer={CustomPickerContainer}
        />
      )}
    </div>
  );
};
