import { FaCheck } from 'react-icons/fa';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';

export const SuccessLogicalModelsInferBadge = () => {
  return (
    <IndicatorCard
      status="positive"
      className="py-4 px-md"
      showIcon
      contentFullWidth
      customIcon={() => <FaCheck />}
    >
      <div className='flex items-center justify-between mx-4"'>
        <div>
          <h1 className="font-bold text-lg">
            Auto-converted the Logical Models from the sample document
          </h1>
          <div className="text-muted">
            Please review details of each Logical Models before tracking
            collection.
          </div>
        </div>
      </div>
    </IndicatorCard>
  );
};
