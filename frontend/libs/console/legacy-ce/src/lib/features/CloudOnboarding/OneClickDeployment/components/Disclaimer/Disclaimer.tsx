import { IndicatorCard } from '@hasura/shared/ui';

export const Disclaimer = () => {
  return (
    <IndicatorCard className="mt-4" status="info" showIcon>
      Please use caution when installing projects provided by third parties. We
      recommend thoroughly reviewing the designated project on GitHub before
      installing.
    </IndicatorCard>
  );
};
