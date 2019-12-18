import PropTypes from 'prop-types';
import React from 'react';
// import { vSetDefaults, vMakeRequest, vExpandHeading } from './ViewActions'; // eslint-disable-line no-unused-vars
// import { setTrigger } from '../EventActions';
import TableHeader from './TableHeader';

import ViewScheduledTriggerRows from './ViewScheduledTriggerRows';

const ViewScheduledTrigger = props => {
  const { dispatch } = props;
  // Choose the right nav bar header thing
  const header = (
    <TableHeader dispatch={dispatch} tabName="scheduledTriggers" />
  );

  return (
    <div>
      {header}
      <br />
      <ViewScheduledTriggerRows dispatch={dispatch} />
    </div>
  );
};

ViewScheduledTrigger.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = state => {
  return {
    ...state,
  };
};

const viewScheduledTriggerConnector = connect =>
  connect(mapStateToProps)(ViewScheduledTrigger);

export default viewScheduledTriggerConnector;
