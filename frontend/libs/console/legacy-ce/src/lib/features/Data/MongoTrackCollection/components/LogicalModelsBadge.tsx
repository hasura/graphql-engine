import { FaInfo } from 'react-icons/fa';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';

export const LogicalModelsBadge = () => {
  return (
    <IndicatorCard
      status="info"
      className="py-4 px-md"
      showIcon
      contentFullWidth
      customIcon={() => <FaInfo />}
    >
      <div className='flex items-center justify-between mx-4"'>
        <div>
          <h1 className="font-bold text-lg">
            A Logical Model is required for your Collection schema.
          </h1>
          <div className="text-muted whitespace-normal">
            Each Collection without validation schema should associate with a
            Logical Model.
          </div>
        </div>
      </div>
    </IndicatorCard>
  );
};
