import React from 'react';

import styles from './ModifyTable.scss';

const TriggerList = ({ tableSchema, dispatch }) => {
  const triggers = tableSchema.triggers;

  if (!triggers.length) {
    return <div>No triggers</div>;
  }

  // TODO add delete trigger btn
  // eslint-disable-next-line no-unused-vars
  const onDelete = () => {
    dispatch();
  };

  return triggers.map((trigger, i) => {
    return (
      <div key={i} className={styles.add_mar_bottom_mid}>
        <div className={styles.add_mar_bottom_small}>
          <b>{trigger.trigger_name}</b>&nbsp;-&nbsp;
          <i>
            {trigger.action_timing} {trigger.event_manipulation}{' '}
            {trigger.action_statement}
          </i>
        </div>
        <div className={styles.text_gray}>{trigger.comment}</div>
      </div>
    );
  });
};

export default TriggerList;
