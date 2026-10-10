import React from 'react';
import { dataRoutes } from '@hasura/shared/utils';
import { ADHOC_EVENTS_HEADING } from '@hasura/shared/types';
import { useAppContext } from '@hasura/shared/context';
import { RelativeLink, TopicDescription } from '@hasura/shared/ui';

const Info: React.FC = () => {
  const { envVars } = useAppContext();
  const topicDescription = (
    <div>
      <p>
        {ADHOC_EVENTS_HEADING} are individual events that can be scheduled to
        reliably trigger a HTTP webhook to run some custom business logic at a
        particular timestamp.
      </p>
      <p>
        You can schedule an event from your backend using the{' '}
        <a
          href="https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/scheduled-triggers.html#create-scheduled-event"
          target="_blank"
          rel="noopener noreferrer"
        >
          create_scheduled_event API
        </a>{' '}
        or through the console from the{' '}
        <RelativeLink to={dataRoutes.getAddAdhocEventRoute('absolute')}>
          Schedule an event
        </RelativeLink>{' '}
        tab.
      </p>
    </div>
  );

  return (
    <div className="pl-0 pt-4 bootstrap-jail">
      <div className="pl-5">
        <TopicDescription
          title="What are Scheduled events?"
          imgUrl={`${envVars.assetsPath}/common/img/scheduled-event.png`}
          imgAlt={ADHOC_EVENTS_HEADING}
          description={topicDescription}
        />
        <hr className="my-6" />
      </div>
    </div>
  );
};

export default Info;
