import { IndicatorCard, IndicatorCardProps } from '../IndicatorCard';
import { DisplayToastErrorMessage } from './DisplayToastErrorMessage';

export const ErrorCard = ({
  error,
  showIcon = true,
  size = '1',
  ...rest
}: Omit<IndicatorCardProps, 'children' | 'status' | 'title'> & {
  error: unknown;
}) => {
  return (
    <IndicatorCard
      status="negative"
      showIcon={showIcon}
      size={size}
      collapsible
      {...rest}
    >
      <DisplayToastErrorMessage message={error} />
    </IndicatorCard>
  );
};
