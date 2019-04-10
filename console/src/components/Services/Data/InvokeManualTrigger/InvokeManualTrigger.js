import React from 'react';
import PropTypes from 'prop-types';

/* This component accepts for following props
 *  1) Trigger name
 *  2) Trigger object
 * It creates a manual trigger and watches the response of the same.
 * */
class InvokeManualTrigger extends React.Component {
  constructor() {
    super();
    this.state = {
      isModalOpen: true,
    };
  }
  render() {
    return null;
  }
}

InvokeManualTrigger.propTypes = {
  args: PropTypes.object.isRequired,
  name: PropTypes.string.isRequired,
  onClose: PropTypes.func.isRequired,
};

export default InvokeManualTrigger;
