import React from 'react';
import DateTimePicker from 'react-datetime';
import 'react-datetime/css/react-datetime.css';
import { IconTooltip } from '../../../../../new-components/Tooltip';
import { useFormContext } from 'react-hook-form';
import { Moment } from 'moment';
import { inputStyles } from '../../../../../components/Services/Events/constants';

export const ScheduledTime = () => {
  const { setValue, watch } = useFormContext();
  const setTime = (value: string | Moment) => {
    setValue('time', value, { shouldValidate: false });
  };
  const time = watch('time');

  return (
    <>
      <div className="block flex items-center text-gray-600 font-semibold mb-xs">
        <label htmlFor="schedule">Time</label>
        <IconTooltip message="The time that this event must be delivered" />
      </div>
      <div className="relative w-full max-w-xl mb-xs" />
      <div className="mb-6">
        <DateTimePicker
          value={time}
          onChange={setTime}
          inputProps={{
            className: `${inputStyles}`,
          }}
        />
      </div>
    </>
  );
};
