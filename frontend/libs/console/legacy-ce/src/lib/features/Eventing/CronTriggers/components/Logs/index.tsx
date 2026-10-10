import React from 'react';
import Logs from './Logs';
import STContainer from '../Container';
import { useParams } from 'react-router';

const ScheduledTriggerLogs: React.FC = () => {
  const { triggerName } = useParams<{ triggerName: string }>();
  if (!triggerName) {
    return <span>Could not find any cron trigger</span>;
  }

  return (
    <STContainer tabName="logs" triggerName={triggerName}>
      {() => <Logs triggerName={triggerName} />}
    </STContainer>
  );
};

export default ScheduledTriggerLogs;
