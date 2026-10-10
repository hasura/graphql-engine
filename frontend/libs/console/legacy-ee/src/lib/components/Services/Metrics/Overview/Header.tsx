import { sub, Duration } from 'date-fns';
import { useState, useEffect } from 'react';
import { FiRefreshCw } from 'react-icons/fi';
import { Button, DropdownButton, DropdownMenu } from '@hasura/shared/ui';

const BucketTypes: { name: string; subtractFactor: Duration }[] = [
  { name: 'Last 10 Minutes', subtractFactor: { minutes: 10 } },
  { name: 'Last Hour ', subtractFactor: { hours: 1 } },
  { name: 'Last 8 Hours', subtractFactor: { hours: 8 } },
  { name: 'Last 12 Hours', subtractFactor: { hours: 12 } },
  { name: 'Last 24 Hours', subtractFactor: { hours: 24 } },
  { name: 'Last 7 Days', subtractFactor: { days: 7 } },
  { name: 'Last 14 Days', subtractFactor: { days: 14 } },
  { name: 'Last 30 Days', subtractFactor: { days: 30 } },
];
const Header = ({ setFromTime }) => {
  const [currentBucket, setCurrentBucket] = useState(BucketTypes[1]);

  useEffect(() => {
    setFromTime(sub(new Date(), currentBucket.subtractFactor).toISOString());
  }, [setFromTime, currentBucket]);

  const reloadData = () =>
    setFromTime(sub(new Date(), currentBucket.subtractFactor).toISOString());

  return (
    <>
      <Button
        size="1"
        mode="default"
        leftIcon={FiRefreshCw}
        onClick={reloadData}
      >
        Refresh
      </Button>
      <DropdownButton
        size="1"
        mode="default"
        items={BucketTypes.map((bucket) => (
          <DropdownMenu.Item
            key={bucket?.name}
            color={currentBucket.name === bucket.name ? 'indigo' : 'gray'}
            onClick={() => {
              setCurrentBucket(bucket);
            }}
          >
            {bucket.name}
          </DropdownMenu.Item>
        ))}
      >
        {currentBucket?.name}
      </DropdownButton>
    </>
  );
};

export default Header;
