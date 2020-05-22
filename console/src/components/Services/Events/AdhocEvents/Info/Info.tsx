import React from 'react';
import globals from '../../../../../Globals';
import styles from '../../Events.scss';
import { ADHOC_EVENTS_HEADING } from '../../constants';
import {
  SAMPLE_SCHEDDULE_EVENT_QUERY,
  SCHEDULED_EVENT_PAYLOAD_EDITOR_MAXLINES,
} from '../utils';
import TopicDescription from '../../../Common/Landing/TopicDescription';
import AceEditor from '../../../../Common/AceEditor/BaseEditor';
import KnowMoreLink from '../../../../Common/KnowMoreLink/KnowMoreLink';

const Info: React.FC = () => {
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
          description={`${ADHOC_EVENTS_HEADING} are individual one-off events that can be scheduled at a particular timestamp. You can schedule an event from your backend using by sending the following payload to /v1/query, or through the console in the "Schedule an event" tab.`}
        />
        <KnowMoreLink href="https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/scheduled-triggers.html#create-scheduled-event" />
        <hr className={styles.clear_fix} />
        <AceEditor
          mode="json"
          value={SAMPLE_SCHEDDULE_EVENT_QUERY}
          readOnly
          maxLines={SCHEDULED_EVENT_PAYLOAD_EDITOR_MAXLINES}
        />
      </div>
    </div>
  );
};

export default Info;
