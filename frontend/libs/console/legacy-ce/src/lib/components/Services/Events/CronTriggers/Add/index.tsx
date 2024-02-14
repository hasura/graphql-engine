import React from 'react';
import Helmet from 'react-helmet';
import { browserHistory } from 'react-router';
import {
  Analytics,
  REDACT_EVERYTHING,
} from '../../../../../features/Analytics';
import { CronTriggers } from '../../../../../features/CronTriggers';

import { getReactHelmetTitle } from '../../../../Common/utils/reactUtils';
import { EVENTS_SERVICE_HEADING, CRON_TRIGGER } from '../../constants';

export const AddConnector: React.FC = () => {
  return (
    <Analytics name="AddScheduledTrigger" {...REDACT_EVERYTHING}>
      <div className="md-md bootstrap-jail">
        <Helmet
          title={getReactHelmetTitle(
            `Create ${CRON_TRIGGER}`,
            EVENTS_SERVICE_HEADING
          )}
        />
        <h2 className="text-subtitle font-bold pt-md pb-md mt-0 mb-0 pl-4">
          Create a new cron trigger
        </h2>
        <CronTriggers.Form
          onSuccess={(triggerName?: string) => {
            browserHistory.push(`/events/cron/${triggerName}/modify`);
          }}
        />
      </div>
    </Analytics>
  );
};

export default AddConnector;
