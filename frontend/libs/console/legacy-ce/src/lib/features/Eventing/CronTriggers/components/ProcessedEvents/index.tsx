import React from 'react';
import { useParams } from 'react-router';
import STContainer from '../Container';
import ProcessedEvents from './ProcessedEvents';

const STProcessedEvents: React.FC = () => {
  const { triggerName } = useParams<{ triggerName: string }>();
  if (!triggerName) {
    return <span>Could not find any cron trigger</span>;
  }

  return (
    <STContainer tabName="processed" triggerName={triggerName}>
      {({ currentTrigger }) => (
        <ProcessedEvents currentTrigger={currentTrigger} />
      )}
    </STContainer>
  );
};

export default STProcessedEvents;
