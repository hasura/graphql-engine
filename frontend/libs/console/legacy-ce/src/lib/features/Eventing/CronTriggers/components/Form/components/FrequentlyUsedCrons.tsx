import React from 'react';
import { Select } from '@hasura/shared/ui';

const FREQ_USED_CRONS = [
  {
    label: 'Every minute',
    value: '* * * * *',
  },
  {
    label: 'Every 10 minutes',
    value: '*/10 * * * *',
  },
  {
    label: 'Every midnight',
    value: '0 0 * * *',
  },
  {
    label: 'Every month start',
    value: '0 0 1 * *',
  },
  {
    label: 'Every friday noon',
    value: '0 12 * * 5',
  },
];

type Props = {
  setCron: (cron: string) => void;
};

const FrequentlyUsedCrons: React.FC<Props> = ({ setCron }) => {
  return (
    <Select
      key="frequently-used-crons"
      onChange={(value) => setCron(value)}
      options={FREQ_USED_CRONS}
      placeholder="Frequently used crons"
    />
  );
};

export default FrequentlyUsedCrons;
