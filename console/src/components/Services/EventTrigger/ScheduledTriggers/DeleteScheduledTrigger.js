import React from 'react';
import useHasuraQuery from './useHasuraQuery';
import { deleteScheduledTriggersQuery } from './Actions';

const DeleteScheduledTrigger = ({ dispatch, name, refetch }) => {
  const { loading, refetch: run } = useHasuraQuery({
    query: deleteScheduledTriggersQuery(name),
    onCompleted: () => refetch(),
    run: false,
    dispatcher: dispatch,
  });
  const styles = require('./ScheduledTrigger.scss');
  const deleteTrigger = () => {
    run();
  };
  const renderIcon = () => {
    if (loading) {
      return <i className="fa fa-spinner fa-spin" />;
    }
    return (
      <i
        onClick={deleteTrigger}
        className={`fa fa-times ${styles.cursorPointer}`}
      />
    );
  };
  return <div className={`${styles.textCenter}`}>{renderIcon()}</div>;
};

export default DeleteScheduledTrigger;
