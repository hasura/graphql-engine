import React from 'react';
import STContainer from '../Container';
import PendingEvents from './PendingEvents';
import { useParams } from 'react-router';
import { IndicatorCard } from '@hasura/shared/ui';

const STPendingEvents: React.FC = () => {
  const { triggerName } = useParams<{ triggerName: string }>();
  if (!triggerName) {
    return (
      <IndicatorCard status="negative" showIcon>
        Could not find any cron trigger
      </IndicatorCard>
    );
  }

  return (
    <STContainer tabName="pending" triggerName={triggerName}>
      {({ currentTrigger }) => (
        <PendingEvents currentTrigger={currentTrigger} />
      )}
    </STContainer>
  );
};

export default STPendingEvents;
