import { IndicatorCard } from '@hasura/shared/ui';
import { Strong } from '@radix-ui/themes';

interface SuccessCardProps {
  routingTo: string;
  value?: string;
}
export const SuccessCard = (props: SuccessCardProps) => {
  const { routingTo, value } = props;
  return (
    <div className="px-6 mb-4">
      <IndicatorCard status="positive" showIcon>
        <div className="mb-2">
          Routing to: <Strong>{routingTo}</Strong>
        </div>
        {value && (
          <div>
            Value: <Strong>{value}</Strong>
          </div>
        )}
      </IndicatorCard>
    </div>
  );
};
