import React from 'react';
import AddScheduledTrigger from './AddScheduledTrigger';

const ScheduledTriggers = ({ dispatch, addScheduledTrigger }) => {
  const styles = require('../TableCommon/EventTable.scss');
  return (
    <div>
      <AddScheduledTrigger
        dispatch={dispatch}
        addScheduledTrigger={addScheduledTrigger}
      />
      <hr />
      <h4 className={styles.subheading_text}>Scheduled triggers</h4>
      <hr />
      <h4 className={styles.subheading_text}>Past runs</h4>
    </div>
  );
};

const mapStateToProps = state => {
  return {
    ...state.scheduledTrigger,
  };
};

const scheduledTriggersConnector = connect =>
  connect(mapStateToProps)(ScheduledTriggers);

export default scheduledTriggersConnector;
