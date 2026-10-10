import { FaInfo } from 'react-icons/fa';
import { IndicatorCard } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

export const LogicalModelsBadge = () => {
  return (
    <IndicatorCard
      status="info"
      className="py-4 px-4"
      showIcon
      customIcon={() => <FaInfo />}
    >
      <Flex align="center" justify="between" className='mx-4"'>
        <div>
          <h1 className="font-bold text-lg">
            A Logical Model is required for your Collection schema.
          </h1>
          <div className="text-muted whitespace-normal">
            Each Collection without validation schema should associate with a
            Logical Model.
          </div>
        </div>
      </Flex>
    </IndicatorCard>
  );
};
