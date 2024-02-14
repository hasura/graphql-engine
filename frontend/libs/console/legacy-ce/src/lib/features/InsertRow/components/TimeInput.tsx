import { Button } from '../../../new-components/Button';
import { Input } from '../../../new-components/Form/Input';
import { ChangeEventHandler, useState } from 'react';
import { FaClock } from 'react-icons/fa';
import DatePicker, { CalendarContainer } from 'react-datepicker';
import { sub } from 'date-fns';
import clsx from 'clsx';
import { CustomEventHandler, TextInputProps } from './TextInput';

import 'react-datepicker/dist/react-datepicker.css';
import './date-input.css';

const CustomPickerContainer: React.FC<{ className: string }> = ({
  className,
  children,
}) => (
  <div className="absolute top-10 left-0 z-50">
    <CalendarContainer className={className}>
      <div style={{ position: 'relative' }}>{children}</div>
    </CalendarContainer>
  </div>
);

const getISOTimePart = (date: Date) => date.toISOString().slice(11, 19);

type TimeInputProps = {
  formatDate?: (date: Date) => string;
} & TextInputProps;

export const TimeInput: React.VFC<TimeInputProps> = ({
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

  const onTimeChange = (date: Date) => {
    const timeZoneOffsetMinutes = date.getTimezoneOffset();
    const utcDate = sub(date, { minutes: timeZoneOffsetMinutes });
    const timeString = formatDate ? formatDate(date) : getISOTimePart(utcDate);

    if (inputRef && 'current' in inputRef && inputRef.current) {
      inputRef.current.value = timeString;
    }
    if (onChange) {
      const changeCb = onChange as CustomEventHandler;
      changeCb({ target: { value: timeString } });
    }
    if (onInput) {
      const inputCb = onInput as CustomEventHandler;
      inputCb({ target: { value: timeString } });
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
              <FaClock
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
          showTimeSelectOnly
          onClickOutside={() => setCalendarPickerVisible(false)}
          onChange={date => {
            if (date) {
              onTimeChange(date);
              setCalendarPickerVisible(false);
            }
          }}
          calendarContainer={CustomPickerContainer}
        />
      )}
    </div>
  );
};
