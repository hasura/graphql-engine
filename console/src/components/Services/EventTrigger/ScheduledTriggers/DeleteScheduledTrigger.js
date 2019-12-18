import React from 'react';
import useHasuraQuery from './useHasuraQuery';
import { deleteScheduledTriggersQuery } from './Actions';

import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import * as tooltip from './Tooltips';

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
      <OverlayTrigger placement="right" overlay={tooltip.deleteScheduleTrigger}>
        <i
          onClick={deleteTrigger}
          className={`fa fa-times ${styles.cursorPointer}`}
        />
      </OverlayTrigger>
    );
  };
  return <div className={`${styles.textCenter}`}>{renderIcon()}</div>;
};

export default DeleteScheduledTrigger;
