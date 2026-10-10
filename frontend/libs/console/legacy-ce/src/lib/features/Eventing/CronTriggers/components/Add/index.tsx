import React from 'react';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { Form } from '../Form';
import { useDocumentTitle } from '@hasura/shared/hooks';
import { useNavigate } from 'react-router';
import { CRON_TRIGGER, EVENTS_SERVICE_HEADING } from '@hasura/shared/types';
import { Heading } from '@radix-ui/themes';
import { dataRoutes } from '@hasura/shared/utils';

export const AddScheduledTrigger: React.FC = () => {
  useDocumentTitle(
    dataRoutes.getReactHelmetTitle(
      `Create ${CRON_TRIGGER}`,
      EVENTS_SERVICE_HEADING,
    ),
  );
  const navigate = useNavigate();

  return (
    <Analytics name="AddScheduledTrigger" {...REDACT_EVERYTHING}>
      <div className="md-md bootstrap-jail">
        <Heading size="4" className="pt-4 pb-4 mt-0 mb-0 pl-4">
          Create a new cron trigger
        </Heading>
        <Form
          onSuccess={(triggerName?: string) => {
            navigate(`/events/cron/${triggerName}/modify`);
          }}
        />
      </div>
    </Analytics>
  );
};

export default AddScheduledTrigger;
