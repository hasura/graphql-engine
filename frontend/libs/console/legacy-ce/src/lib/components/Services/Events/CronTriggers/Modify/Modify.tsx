import React from 'react';
import {
  Analytics,
  REDACT_EVERYTHING,
} from '../../../../../features/Analytics';
import { CronTriggers } from '../../../../../features/CronTriggers';
import { browserHistory } from 'react-router';
import { ScheduledTrigger } from '../../types';

type Props = {
  currentTrigger?: ScheduledTrigger;
};

const Modify: React.FC<Props> = props => {
  const name = props.currentTrigger?.name;
  return (
    <Analytics name="ScheduledTri<EggerModify" {...REDACT_EVERYTHING}>
      <div className="mb-md bootstrap-jail">
        <CronTriggers.Form
          key={name}
          cronTriggerName={name}
          onSuccess={(triggerName?: string) => {
            browserHistory.push(`/events/cron/${triggerName}/modify`);
          }}
          onDeleteSuccess={() => browserHistory.push('/events/cron')}
        />
      </div>
    </Analytics>
  );
};

export default Modify;
