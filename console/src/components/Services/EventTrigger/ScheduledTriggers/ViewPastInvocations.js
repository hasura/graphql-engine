import PropTypes from 'prop-types';
import React, { Component } from 'react';
import TableHeader from './TableHeader';
import ViewPastInvocationRows from './ViewPastInvocationRows';

class ViewPastInvocations extends Component {
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
    const header = (
      <TableHeader dispatch={dispatch} tabName="pastInvocations" />
    );

    return (
      <div>
        {header}
        <br />
        <ViewPastInvocationRows dispatch={dispatch} />
      </div>
    );
  }
}

ViewPastInvocations.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = state => {
  return {
    ...state,
  };
};

const viewScheduledTriggerConnector = connect =>
  connect(mapStateToProps)(ViewPastInvocations);

export default viewScheduledTriggerConnector;
