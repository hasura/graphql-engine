import { InputField } from '@hasura/shared/ui';
import FrequentlyUsedCrons from './FrequentlyUsedCrons';
import { useFormContext } from 'react-hook-form';

const defaultCronExpr = '* * * * *';

export const CronScheduleSelector = () => {
  const { setValue } = useFormContext();
  const setCron = (value: string) => {
    setValue('schedule', value, { shouldValidate: true });
  };

  return (
    <div className="relative w-full">
      <InputField
        name="schedule"
        label="Cron Schedule"
        tooltip="Schedule for your cron (events are created based on the UTC timezone)"
        learnMoreLink="https://crontab.guru/#*_*_*_*_*"
        learnMoreLinkText="(Build a cron expression)"
        fieldProps={{ type: 'text', placeholder: defaultCronExpr }}
      />
      <div className="my-4">
        <FrequentlyUsedCrons setCron={setCron} />
      </div>
    </div>
  );
};
