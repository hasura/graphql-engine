import { IconButton } from '@hasura/shared/ui';
import { ChangeEventHandler, useState } from 'react';
import { FaCalendar } from 'react-icons/fa';
import DatePicker, { CalendarContainer } from 'react-datepicker';
import { format } from 'date-fns';
import { CustomEventHandler, TextInput, TextInputProps } from './TextInput';

export const DateInput: React.FC<TextInputProps> = ({
  name,
  disabled,
  placeholder,
  ref,
  onChange,
  onInput,
  onBlur,
  defaultValue,
  ...rest
}) => {
  const [isCalendarPickerVisible, setCalendarPickerVisible] = useState(false);

  const onDateChange = (date: Date | null) => {
    if (!date) {
      return;
    }

    const dateString = format(date, 'yyyy-MM-dd');

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
          onSelect={() => setCalendarPickerVisible(false)}
          onClickOutside={() => setCalendarPickerVisible(false)}
          onChange={onDateChange}
          calendarContainer={CustomPickerContainer}
        />
      )}
    </div>
  );
};

const CustomPickerContainer = ({ className, children }) => {
  return (
    <div className="absolute top-10 left-0 z-50">
      <CalendarContainer className={className}>
        <div style={{ position: 'relative' }}>{children}</div>
      </CalendarContainer>
    </div>
  );
};
