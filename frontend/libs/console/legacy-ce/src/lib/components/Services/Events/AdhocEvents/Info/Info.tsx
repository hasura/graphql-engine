import React from 'react';
import { Link } from 'react-router';
import globals from '../../../../../Globals';
import { ADHOC_EVENTS_HEADING } from '../../constants';
import TopicDescription from '../../../Common/Landing/TopicDescription';
import { getAddAdhocEventRoute } from '../../../../Common/utils/routesUtils';

const Info: React.FC = () => {
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
        <Link to={getAddAdhocEventRoute('absolute')}>Schedule an event</Link>{' '}
        tab.
      </p>
    </div>
  );

  return (
    <div className="pl-0 pt-md bootstrap-jail">
      <div className="pl-5">
        <TopicDescription
          title="What are Scheduled events?"
          imgUrl={`${globals.assetsPath}/common/img/scheduled-event.png`}
          imgAlt={ADHOC_EVENTS_HEADING}
          description={topicDescription}
        />
        <hr className="my-lg" />
      </div>
    </div>
  );
};

export default Info;
