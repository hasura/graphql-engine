import React from 'react';
import PropTypes from 'prop-types';

class InvokeTrigger extends React.Component {
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

InvokeTrigger.propTypes = {
  args: PropTypes.object.isRequired,
};

export default InvokeTrigger;
