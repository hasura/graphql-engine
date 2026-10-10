import React from 'react';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { Form } from '../Form';
import { useNavigate } from 'react-router';
import { CronTrigger } from '@hasura/shared/types';

type Props = {
  currentTrigger: CronTrigger;
};

const Modify: React.FC<Props> = ({ currentTrigger }) => {
  const navigate = useNavigate();

  return (
    <Analytics name="ScheduledTriggerModify" {...REDACT_EVERYTHING}>
      <div className="mb-4">
        <Form
          currentTrigger={currentTrigger}
          onSuccess={(triggerName?: string) => {
            navigate(`/events/cron/${triggerName}/modify`);
          }}
          onDeleteSuccess={() => navigate('/events/cron')}
        />
      </div>
    </Analytics>
  );
};

export default Modify;
