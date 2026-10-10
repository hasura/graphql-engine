import { IconButton, InputProps } from '@hasura/shared/ui';

import {
  ChangeEventHandler,
  FormEventHandler,
  forwardRef,
  useState,
} from 'react';
import { FaClock } from 'react-icons/fa';
import DatePicker, { CalendarContainer } from 'react-datepicker';
import { sub } from 'date-fns';

import { CustomEventHandler, TextInput } from './TextInput';

const CustomPickerContainer: React.FC<{
  className: string;
  children?: React.ReactNode;
}> = ({ className, children }) => (
  <div className="absolute top-10 left-0 z-50">
    <CalendarContainer className={className}>
      <div style={{ position: 'relative' }}>{children}</div>
    </CalendarContainer>
  </div>
);

const getISOTimePart = (date: Date) => date.toISOString().slice(11, 19);

type TimeInputProps = {
  formatDate?: (date: Date) => string;
} & InputProps;

export const TimeInput = forwardRef(
  (
    {
      name,
      disabled,
      placeholder,
      onChange,
      onInput,
      onBlur,
      formatDate,
      defaultValue,
      ...rest
    }: TimeInputProps,
    ref,
  ) => {
    const [isCalendarPickerVisible, setCalendarPickerVisible] = useState(false);

    const onTimeChange = (date: Date) => {
      const timeZoneOffsetMinutes = date.getTimezoneOffset();
      const utcDate = sub(date, { minutes: timeZoneOffsetMinutes });
      const timeString = formatDate
        ? formatDate(date)
        : getISOTimePart(utcDate);

      if (ref && 'current' in ref && ref.current) {
        (ref.current as HTMLInputElement).value = timeString;
      }
      if (onChange) {
        const changeCb = onChange as CustomEventHandler;
        changeCb({ target: { value: timeString } });
      }

      if (onInput) {
        const inputCb = onInput as unknown as CustomEventHandler;
        inputCb({ target: { value: timeString } });
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
          onInput={onInput as FormEventHandler<HTMLInputElement>}
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
              <FaClock />
            </IconButton>
          }
          disabled={disabled}
        />
        {isCalendarPickerVisible && (
          <DatePicker
            inline
            showTimeSelect
            showTimeSelectOnly
            onClickOutside={() => setCalendarPickerVisible(false)}
            onChange={(date) => {
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
  },
);
