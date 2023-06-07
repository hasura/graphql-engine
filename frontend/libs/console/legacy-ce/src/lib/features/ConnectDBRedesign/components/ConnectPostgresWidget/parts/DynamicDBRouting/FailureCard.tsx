import { FaTimesCircle } from 'react-icons/fa';
import { IndicatorCard } from '../../../../../../new-components/IndicatorCard';

interface FailureCardProps {
  message: string;
}
export const FailureCard = (props: FailureCardProps) => {
  const { message } = props;
  return (
    <div className="px-6 mb-4">
      <IndicatorCard status="negative">
        <div className="flex flex-col">
          <div className="mb-2">
            <span className="font-semibold text-red-600">
              <FaTimesCircle className="mr-3" /> Your request failed:
            </span>
          </div>
          <div className="pl-8">
            <div className="mb-2">{message}</div>
          </div>
        </div>
      </IndicatorCard>
    </div>
  );
};
