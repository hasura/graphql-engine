import PropTypes from 'prop-types';
import React, { Component } from 'react';
// import { vSetDefaults, vMakeRequest, vExpandHeading } from './ViewActions'; // eslint-disable-line no-unused-vars
// import { setTrigger } from '../EventActions';
import TableHeader from './TableHeader';
// import ViewRows from './ViewRows';
// import { NotFoundError } from '../../../Error/PageNotFound';

class ViewUpcomingEvents extends Component {
  constructor(props) {
    super(props);
    // Initialize this table
    this.state = {
      dispatch: props.dispatch,
    };
  }

  render() {
    const { dispatch } = this.props;
    // Choose the right nav bar header thing
    const header = <TableHeader dispatch={dispatch} tabName="upcomingEvents" />;

    return (
      <div>
        {header}
        <br />
        Upcoming invocations
      </div>
    );
  }
}

ViewUpcomingEvents.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = state => {
  return {
    ...state,
  };
};

const viewUpcomingEventsConnector = connect =>
  connect(mapStateToProps)(ViewUpcomingEvents);

export default viewUpcomingEventsConnector;
