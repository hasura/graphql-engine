import React from 'react';
import { IconTooltip } from '@/new-components/Tooltip';
import { InputField } from '@/new-components/Form';
import FrequentlyUsedCrons from '@/components/Services/Events/Common/Components/FrequentlyUsedCrons';
import { useFormContext } from 'react-hook-form';

const defaultCronExpr = '* * * * *';

export const CronScheduleSelector = () => {
  const { setValue } = useFormContext();
  const setCron = (value: string) => {
    setValue('schedule', value, { shouldValidate: true });
  };

  return (
    <>
      <div className="block flex items-center text-gray-600 font-semibold">
        <label htmlFor="schedule">Cron Schedule</label>
        <IconTooltip message="Schedule for your cron (events are created based on the UTC timezone)" />
        <a
          className="ml-xs cursor-pointer hover:no-underline font-normal text-secondary hover:text-secondary-darker italic"
          href="https://crontab.guru/#*_*_*_*_*"
          target="_blank"
          rel="noopener noreferrer"
        >
          (Build a cron expression)
        </a>
      </div>
      <div className="relative w-full">
        <InputField name="schedule" type="text" placeholder={defaultCronExpr} />
        <div className="-mt-xs mb-md">
          <FrequentlyUsedCrons setCron={setCron} />
        </div>
      </div>
    </>
  );
};
