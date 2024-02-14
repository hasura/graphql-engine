import { FaCheckCircle } from 'react-icons/fa';
import { IndicatorCard } from '../../../../../../new-components/IndicatorCard';

interface SuccessCardProps {
  routingTo: string;
  value?: string;
}
export const SuccessCard = (props: SuccessCardProps) => {
  const { routingTo, value } = props;
  return (
    <div className="px-6 mb-4">
      <IndicatorCard status="positive">
        <div className="flex flex-col">
          <div className="mb-2">
            <span className="font-semibold text-green-600">
              <FaCheckCircle className="mr-3" /> Your request succeeded:
            </span>
          </div>
          <div className="pl-8">
            <div className="mb-2">
              Routing to: <span className="font-semibold">{routingTo}</span>
            </div>
            {value && (
              <div>
                Value: <span className="font-semibold">{value}</span>
              </div>
            )}
          </div>
        </div>
      </IndicatorCard>
    </div>
  );
};
