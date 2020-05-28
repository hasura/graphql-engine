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
      {ADHOC_EVENTS_HEADING} are individual one-off events that can be scheduled
      at a particular timestamp. You can schedule an event from your
      backend&nbsp;
      <a
        href="https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/scheduled-triggers.html#create-scheduled-event"
        target="_blank"
        rel="noopener noreferrer"
      >
        using the metadata api
      </a>
      , or&nbsp;
      <Link to={getAddAdhocEventRoute('absolute')}>through the console</Link>.
    </div>
  );

  return (
    <div
      className={`${styles.padd_left_remove} container-fluid ${styles.padd_top}`}
    >
      <div className={styles.padd_left}>
        <div className={styles.display_flex}>
          <h2 className={`${styles.headerText} ${styles.inline_block}`}>
            {ADHOC_EVENTS_HEADING}
          </h2>
        </div>
        <hr />
        <TopicDescription
          title="What are Cron Triggers?"
          imgUrl={`${globals.assetsPath}/common/img/event-trigger.png`}
          imgAlt={ADHOC_EVENTS_HEADING}
          description={topicDescription}
        />
        <hr className={styles.clear_fix} />
      </div>
    </div>
  );
};

export default Info;
