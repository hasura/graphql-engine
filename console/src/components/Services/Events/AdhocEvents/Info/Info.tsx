import React from 'react';
import { Link } from 'react-router';
import globals from '../../../../../Globals';
import styles from '../../Events.scss';
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
    <div
      className={`${styles.padd_left_remove} container-fluid ${styles.padd_top}`}
    >
      <div className={styles.padd_left}>
        <TopicDescription
          title="What are Scheduled events?"
          imgUrl={`${globals.assetsPath}/common/img/scheduled-event.png`}
          imgAlt={ADHOC_EVENTS_HEADING}
          description={topicDescription}
        />
        <hr className={`${styles.clear_fix} my-lg`} />
      </div>
    </div>
  );
};

export default Info;
