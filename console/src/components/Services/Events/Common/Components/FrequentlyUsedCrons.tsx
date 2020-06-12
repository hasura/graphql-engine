import React from 'react';
import Dropdown from '../../../../Common/Dropdown/Dropdown';
import Button from '../../../../Common/Button/Button';

const FREQ_USED_CRONS = [
  {
    name: 'Every minute',
    value: '* * * * *',
  },
  {
    name: 'Every 10 minutes',
    value: '*/10 * * * *',
  },
  {
    name: 'Every midnight',
    value: '0 0 * * *',
  },
  {
    name: 'Every month start',
    value: '0 0 1 * *',
  },
  {
    name: 'Every friday noon',
    value: '0 12 * * 5',
  },
];

type Props = {
  setCron: (cron: string) => void;
};

const FrequentlyUsedCrons: React.FC<Props> = ({ setCron }) => {
  const options = FREQ_USED_CRONS.map(cron => {
    return {
      content: (
        <div>
          <b>{cron.name}</b>
          <div>{cron.value}</div>
        </div>
      ),
      onClick: () => setCron(cron.value),
    };
  });
  return (
    <Dropdown
      testId="frequently-used-crons"
      options={options}
      position="bottom"
      key="frequently-used-crons"
      keyPrefix="frequently-used-crons"
    >
      <Button color="white" size="xs">
        Frequently used crons
      </Button>
    </Dropdown>
  );
};

export default FrequentlyUsedCrons;
