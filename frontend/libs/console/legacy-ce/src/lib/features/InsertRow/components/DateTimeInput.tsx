import { Button } from '../../../new-components/Button';
import { Input } from '../../../new-components/Form/Input';
import { ChangeEventHandler, useState } from 'react';
import { FaCalendar } from 'react-icons/fa';
import DatePicker, { CalendarContainer } from 'react-datepicker';
import clsx from 'clsx';
import { sub } from 'date-fns';
import { CustomEventHandler, TextInputProps } from './TextInput';

import 'react-datepicker/dist/react-datepicker.css';
import './date-input.css';

const CustomPickerContainer: React.FC<{ className: string }> = ({
  className,
  children,
}) => (
  <div className="absolute top-10 left-0 z-50 min-w-[300px]">
    <CalendarContainer className={className}>
      <div style={{ position: 'relative' }}>{children}</div>
    </CalendarContainer>
  </div>
);

type DateTimeInputProps = {
  formatDate?: (date: Date) => string;
} & TextInputProps;

export const DateTimeInput: React.VFC<DateTimeInputProps> = ({
  name,
  disabled,
  placeholder,
  inputRef,
  onChange,
  onInput,
  onBlur,
  formatDate,
}) => {
  const [isCalendarPickerVisible, setCalendarPickerVisible] = useState(false);

  const onDateTimeChange = (date: Date) => {
    const timeZoneOffsetMinutes = date.getTimezoneOffset();
    const utcDate = sub(date, { minutes: timeZoneOffsetMinutes });
    const dateString = formatDate ? formatDate(date) : utcDate.toISOString();

    if (inputRef && 'current' in inputRef && inputRef.current) {
      inputRef.current.value = dateString;
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
      <Input
        className="w-full"
        name={name}
        label={name}
        type="text"
        placeholder={placeholder}
        onChange={onChange as ChangeEventHandler<HTMLInputElement>}
        onInput={onInput as ChangeEventHandler<HTMLInputElement>}
        inputProps={{
          onBlur: onBlur,
          ref: inputRef,
        }}
        rightButton={
          <Button
            icon={
              <FaCalendar
                className={clsx(
                  isCalendarPickerVisible && 'fill-yellow-600 stroke-yellow-600'
                )}
              />
            }
            disabled={disabled}
            onClick={() => {
              if (disabled) {
                return;
              }
              setCalendarPickerVisible(!isCalendarPickerVisible);
            }}
          />
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
